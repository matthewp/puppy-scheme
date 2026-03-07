import mod from "../dist/homepage.wasm";

function readString(memory, ptr, len) {
  return new TextDecoder().decode(new Uint8Array(memory.buffer, ptr, len));
}

function writeString(memory, realloc, str) {
  const encoded = new TextEncoder().encode(str);
  const ptr = realloc(0, 0, 1, encoded.length);
  new Uint8Array(memory.buffer).set(encoded, ptr);
  return [ptr, encoded.length];
}

export default {
  async fetch(request, env, ctx) {
    const cache = caches.default;
    const handles = new Map();
    let nextHandle = 1;
    let pendingHandles = new Set();

    function allocHandle(value) {
      const h = nextHandle++;
      handles.set(h, value);
      pendingHandles.add(h);
      return h;
    }

    const instance = new WebAssembly.Instance(mod, {
      env: {
        "raw-http-get"(urlPtr, urlLen) {
          const url = readString(instance.exports.memory, urlPtr, urlLen);
          return allocHandle(fetch(url, {
            headers: { "User-Agent": "puppy-scheme-website" },
          }));
        },
        "raw-response-status"(handle) {
          return handles.get(handle).status;
        },
        "raw-response-text"(handle) {
          return allocHandle(handles.get(handle).text());
        },
        "raw-get-string"(handle, retPtr) {
          const [ptr, len] = writeString(
            instance.exports.memory,
            instance.exports.cabi_realloc,
            handles.get(handle),
          );
          const view = new DataView(instance.exports.memory.buffer);
          view.setInt32(retPtr, ptr, true);
          view.setInt32(retPtr + 4, len, true);
        },
        "raw-json-get"(jsonPtr, jsonLen, keyPtr, keyLen, retPtr) {
          const json = readString(instance.exports.memory, jsonPtr, jsonLen);
          const key = readString(instance.exports.memory, keyPtr, keyLen);
          const value = String(JSON.parse(json)[key] ?? "");
          const [ptr, len] = writeString(
            instance.exports.memory,
            instance.exports.cabi_realloc,
            value,
          );
          const view = new DataView(instance.exports.memory.buffer);
          view.setInt32(retPtr, ptr, true);
          view.setInt32(retPtr + 4, len, true);
        },
        "raw-cache-get"(urlPtr, urlLen) {
          const url = readString(instance.exports.memory, urlPtr, urlLen);
          return allocHandle(cache.match(url));
        },
        "raw-cache-hit"(handle) {
          return handles.get(handle) !== undefined ? 1 : 0;
        },
        "raw-cache-put"(urlPtr, urlLen, responseHandle, ttl) {
          const url = readString(instance.exports.memory, urlPtr, urlLen);
          const response = handles.get(responseHandle).clone();
          const cached = new Response(response.body, {
            headers: {
              ...Object.fromEntries(response.headers),
              "Cache-Control": `public, max-age=${ttl}`,
            },
          });
          ctx.waitUntil(cache.put(url, cached));
        },
      },
    });

    instance.exports._start();
    instance.exports.load();

    while (pendingHandles.size > 0) {
      const batch = [...pendingHandles];
      pendingHandles = new Set();

      for (const handle of batch) {
        handles.set(handle, await handles.get(handle));
      }
      for (const handle of batch) {
        instance.exports["deliver-promise-result"](handle);
      }
    }

    const retPtr = instance.exports["render-page"]();
    const view = new DataView(instance.exports.memory.buffer);
    const ptr = view.getInt32(retPtr, true);
    const len = view.getInt32(retPtr + 4, true);
    const bytes = new Uint8Array(instance.exports.memory.buffer, ptr, len);
    return new Response(bytes, {
      headers: {
        "Content-Type": "text/html; charset=utf-8",
        "Content-Length": len,
      },
    });
  },
};
