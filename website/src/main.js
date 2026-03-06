import { createApp, createHost } from './puppykit.js';

async function boot() {
  const resp = await fetch('app.wasm');
  const bytes = await resp.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes);
  instance.exports._start();

  const app = createApp(instance);
  const host = createHost(document.getElementById('app'), app);
  host.mount(app.root());

  if (typeof Prism !== 'undefined') Prism.highlightAll();
}

boot();
