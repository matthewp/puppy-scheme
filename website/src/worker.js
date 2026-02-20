import mod from "../dist/homepage.wasm";

export default {
  fetch() {
    const instance = new WebAssembly.Instance(mod);
    instance.exports._start();
    const [ptr, len] = instance.exports["render-page"]();
    const bytes = new Uint8Array(instance.exports.memory.buffer, ptr, len);
    return new Response(bytes, {
      headers: {
        "Content-Type": "text/html; charset=utf-8",
        "Content-Length": len,
      },
    });
  },
};
