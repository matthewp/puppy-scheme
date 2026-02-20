export function createHost(container) {
  function mount(renderer) {
    const comp = {
      renderer,
      mountPoint: container,
      slots: [],
      initialized: false,
    };
    renderComponent(comp);
    return comp;
  }

  function renderComponent(comp) {
    const opcodes = comp.renderer.render();
    if (!comp.initialized) {
      if (comp.mountPoint.childNodes.length > 0) {
        hydrateDOM(opcodes, comp, comp.mountPoint);
      } else {
        buildDOM(opcodes, comp, comp.mountPoint);
      }
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
          element.addEventListener(eventType, () => {
            comp.renderer.handleEvent(handlerName);
            renderComponent(comp);
          });
          break;
        }
      }
    }
  }

  function hydrateDOM(opcodes, comp, parent) {
    const stack = [];
    let current = parent;
    let childIdx = 0;
    let element = null;

    for (const op of opcodes) {
      switch (op.tag) {
        case 'open': {
          const el = current.childNodes[childIdx];
          stack.push({ node: current, idx: childIdx });
          current = el;
          element = el;
          childIdx = 0;
          break;
        }
        case 'close': {
          const frame = stack.pop();
          current = frame.node;
          childIdx = frame.idx + 1;
          break;
        }
        case 'attr':
          break;
        case 'text':
          childIdx++;
          break;
        case 'slot': {
          const node = current.childNodes[childIdx];
          comp.slots.push(node);
          childIdx++;
          break;
        }
        case 'event': {
          const [eventType, handlerName] = op.val;
          element.addEventListener(eventType, () => {
            comp.renderer.handleEvent(handlerName);
            renderComponent(comp);
          });
          break;
        }
      }
    }
  }

  function updateDOM(opcodes, comp) {
    let slotIdx = 0;
    for (const op of opcodes) {
      if (op.tag === 'slot') {
        const node = comp.slots[slotIdx++];
        if (node.textContent !== op.val) {
          node.textContent = op.val;
        }
      }
    }
  }

  return { mount };
}
