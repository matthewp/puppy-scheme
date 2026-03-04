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
