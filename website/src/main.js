import { createHost } from './runtime.js';

const TAG_NAMES = ['open', 'close', 'attr', 'text', 'slot', 'event'];

async function boot() {
  const resp = await fetch('counter.wasm');
  const bytes = await resp.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes);

  const memory = instance.exports.memory;
  const _start = instance.exports._start;
  const render = instance.exports.render;
  const wasmHandleEvent = instance.exports["dispatch-handler"];
  const alloc = instance.exports.alloc;

  function readString(ptr, len) {
    return new TextDecoder().decode(new Uint8Array(memory.buffer, ptr, len));
  }

  function readOpcodes(retPtr) {
    const view = new DataView(memory.buffer);
    const arrayPtr = view.getUint32(retPtr, true);
    const count = view.getUint32(retPtr + 4, true);
    const opcodes = [];
    for (let i = 0; i < count; i++) {
      const off = arrayPtr + i * 32;
      const tag = TAG_NAMES[view.getUint8(off)];
      const f0ptr = view.getUint32(off + 8, true);
      const f0len = view.getUint32(off + 12, true);
      const f1ptr = view.getUint32(off + 16, true);
      const f1len = view.getUint32(off + 20, true);
      const f0 = f0len > 0 ? readString(f0ptr, f0len) : null;
      const f1 = f1len > 0 ? readString(f1ptr, f1len) : null;
      switch (tag) {
        case 'open':
        case 'text':
        case 'slot':
          opcodes.push({ tag, val: f0 });
          break;
        case 'close':
          opcodes.push({ tag });
          break;
        case 'attr':
        case 'event':
          opcodes.push({ tag, val: [f0, f1] });
          break;
      }
    }
    return opcodes;
  }

  function writeString(str) {
    const encoded = new TextEncoder().encode(str);
    const ptr = alloc(encoded.length);
    new Uint8Array(memory.buffer, ptr, encoded.length).set(encoded);
    return [ptr, encoded.length];
  }

  _start();

  const renderer = {
    render() {
      const retPtr = render();
      return readOpcodes(retPtr);
    },
    handleEvent(name) {
      const [ptr, len] = writeString(name);
      wasmHandleEvent(ptr, len);
    }
  };

  const host = createHost(document.getElementById('app'));
  host.mount(renderer);
}

boot();
