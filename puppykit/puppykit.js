const TAG_NAMES = ['open', 'close', 'attr', 'text', 'slot', 'event', 'attr-slot', 'component'];

export function createApp(instance) {
  const memory = instance.exports.memory;
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

  return {
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
    root() {
      return wasmRoot();
    },
  };
}

export function createHost(container, wasm) {
  function mount(typeId) {
    container.innerHTML = '';
    const instanceId = wasm.create(typeId);
    const comp = {
      instanceId,
      mountPoint: container,
      slots: [],
      attrSlots: [],
      children: [],
      initialized: false,
    };
    renderComponent(comp);
    return comp;
  }

  function renderComponent(comp) {
    const opcodes = wasm.render(comp.instanceId);
    if (!comp.initialized) {
      buildDOM(opcodes, comp, comp.mountPoint);
      comp.initialized = true;
    } else {
      updateDOM(opcodes, comp);
    }
  }

  function buildDOM(opcodes, comp, parent) {
    const stack = [];
    let current = parent;
    let element = null;

    for (const op of opcodes) {
      switch (op.tag) {
        case 'open': {
          const el = document.createElement(op.val);
          current.appendChild(el);
          stack.push(current);
          current = el;
          element = el;
          break;
        }
        case 'close':
          current = stack.pop();
          break;
        case 'attr':
          element.setAttribute(op.val[0], op.val[1]);
          break;
        case 'attr-slot':
          element.setAttribute(op.val[0], op.val[1]);
          comp.attrSlots.push({ element, name: op.val[0] });
          break;
        case 'text':
          current.appendChild(document.createTextNode(op.val));
          break;
        case 'slot': {
          const node = document.createTextNode(op.val);
          current.appendChild(node);
          comp.slots.push(node);
          break;
        }
        case 'event': {
          const [eventType, handlerName] = op.val;
          const targetComp = comp;
          element.addEventListener(eventType, () => {
            wasm.dispatch(targetComp.instanceId, handlerName);
            renderComponent(targetComp);
          });
          break;
        }
        case 'component': {
          const childTypeId = op.val;
          const childInstanceId = wasm.create(childTypeId);
          const childComp = {
            instanceId: childInstanceId,
            mountPoint: current,
            slots: [],
            attrSlots: [],
            children: [],
            initialized: false,
          };
          const childOpcodes = wasm.render(childInstanceId);
          buildDOM(childOpcodes, childComp, current);
          childComp.initialized = true;
          comp.children.push(childComp);
          break;
        }
      }
    }
  }

  function updateDOM(opcodes, comp) {
    let slotIdx = 0;
    let attrSlotIdx = 0;
    for (const op of opcodes) {
      if (op.tag === 'slot') {
        const node = comp.slots[slotIdx++];
        if (node.textContent !== op.val) {
          node.textContent = op.val;
        }
      } else if (op.tag === 'attr-slot') {
        const entry = comp.attrSlots[attrSlotIdx++];
        if (entry.element.getAttribute(entry.name) !== op.val[1]) {
          entry.element.setAttribute(entry.name, op.val[1]);
        }
      }
    }
  }

  return { mount };
}
