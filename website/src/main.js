import { createHost } from './runtime.js';

const TAG_NAMES = ['open', 'close', 'attr', 'text', 'slot', 'event', 'attr-slot', 'component'];

async function boot() {
  const resp = await fetch('app.wasm');
  const bytes = await resp.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes);

  const memory = instance.exports.memory;
  const _start = instance.exports._start;
  const wasmCreate = instance.exports.create;
  const wasmRender = instance.exports.render;
  const wasmDispatch = instance.exports.dispatch;
  const alloc = instance.exports.alloc;
  const wasmRoot = instance.exports.root;

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
      switch (tag) {
        case 'open':
        case 'text':
        case 'slot':
          opcodes.push({ tag, val: readString(f0ptr, f0len) });
          break;
        case 'close':
          opcodes.push({ tag });
          break;
        case 'attr':
        case 'event':
          opcodes.push({ tag, val: [readString(f0ptr, f0len), readString(f1ptr, f1len)] });
          break;
        case 'attr-slot':
          opcodes.push({ tag, val: [readString(f0ptr, f0len), readString(f1ptr, f1len)] });
          break;
        case 'component':
          opcodes.push({ tag, val: f0ptr });
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

  const wasm = {
    create(typeId) {
      return wasmCreate(typeId);
    },
    render(instanceId) {
      const retPtr = wasmRender(instanceId);
      return readOpcodes(retPtr);
    },
    dispatch(instanceId, event) {
      const [ptr, len] = writeString(event);
      wasmDispatch(instanceId, ptr, len);
    },
  };

  const rootTypeId = wasmRoot();
  const host = createHost(document.getElementById('app'), wasm);
  host.mount(rootTypeId);
  if (typeof Prism !== 'undefined') Prism.highlightAll();
}

boot();
