# HTTP Support

## Context

[wasi-http](https://github.com/WebAssembly/wasi-http) (`wasi:http@0.2.x`) defines typed interfaces for sending and receiving HTTP requests in WebAssembly components. It's Phase 3 (implementation), supported by wasmtime, and is the standard way to build HTTP servers and clients in WASM.

Puppy Scheme should support HTTP with a single component binary that works everywhere. The Scheme API is the same regardless of host. The compiler handles the impedance mismatch.

## wasi:http Architecture

### Worlds

The `proxy` world is the main entry point:

```wit
world proxy {
  // Imports (component can make outbound requests)
  import wasi:http/outgoing-handler;
  import wasi:clocks/monotonic-clock;
  import wasi:cli/stdout;
  import wasi:cli/stderr;
  import wasi:random/random;

  // Export (host delivers inbound requests here)
  export wasi:http/incoming-handler;
}
```

### Key Interfaces

**`incoming-handler`** (exported — handle requests):
```wit
interface incoming-handler {
  handle: func(request: incoming-request, response-out: response-outparam);
}
```

**`outgoing-handler`** (imported — make requests):
```wit
interface outgoing-handler {
  handle: func(request: outgoing-request, options: option<request-options>)
    -> result<future-incoming-response, error-code>;
}
```

### Resource Types

The API is resource-heavy. Every HTTP concept is a resource with ownership semantics:

| Resource | Role |
|---|---|
| `fields` | Mutable/immutable header/trailer map. Methods: `get`, `set`, `append`, `delete`, `entries`, `clone`, `from-list` |
| `incoming-request` | Read request metadata + consume body. Methods: `method`, `path-with-query`, `scheme`, `authority`, `headers`, `consume` |
| `outgoing-request` | Build request to send. Constructor takes headers. Methods: `set-method`, `set-path-with-query`, `set-scheme`, `set-authority`, `body` |
| `incoming-response` | Read response metadata + consume body. Methods: `status`, `headers`, `consume` |
| `outgoing-response` | Build response to send. Constructor takes headers, default status 200. Methods: `set-status-code`, `body` |
| `incoming-body` | Stream reader. Methods: `%stream` → `input-stream`, `finish` → `future-trailers` |
| `outgoing-body` | Stream writer. Methods: `write` → `output-stream`, `finish` (consumes body) |
| `response-outparam` | Capability to send exactly one response. `set` consumes it |
| `request-options` | Transport timeouts: connect, first-byte, between-bytes |
| `future-incoming-response` | Async response handle. `subscribe` → `pollable`, `get` → nested result |
| `future-trailers` | Async trailers handle. Same pattern |

### Data Flow

**Incoming (server):**
1. Host calls `handle(request, response-out)`
2. Read: `request.method()`, `request.path-with-query()`, `request.headers()`
3. Read body: `request.consume()` → `incoming-body` → `%stream()` → `input-stream` → read bytes
4. Build response: `outgoing-response(headers)`, `set-status-code`, `body()` → `outgoing-body` → `write()` → `output-stream` → write bytes → `finish`
5. Send: `response-outparam.set(response-out, ok(response))`

**Outgoing (client):**
1. Build: `outgoing-request(headers)`, set method/path/scheme/authority, optionally write body
2. Send: `outgoing-handler.handle(request, options)` → `future-incoming-response`
3. Wait: `future.subscribe()` → `pollable` → `block()`
4. Read: `future.get()` → unwrap → `incoming-response` → status, headers, body stream

### Dependencies

wasi:http imports from:
- `wasi:io/streams@0.2.x` — `input-stream`, `output-stream` (body content)
- `wasi:io/poll@0.2.x` — `pollable` (async waiting)
- `wasi:io/error@0.2.x` — `error` resource (downcasted via `http-error-code`)
- `wasi:clocks/monotonic-clock@0.2.x` — `duration` (for timeouts)

## Async Model: Callbacks

### The problem

WASI HTTP is inherently async — `outgoing-handler.handle` returns a `future-incoming-response`, not a response. On wasmtime (native), you can call `pollable.block()` to synchronously wait. But on the web, blocking is impossible — JS is single-threaded, and if WASM blocks, the event loop can't run `fetch()` to completion.

JSPI (JS-Promise Integration) solves this at the engine level but has limited browser support. We shouldn't depend on it.

### Callbacks are idiomatic Scheme

The preferred approach is CPS (continuation-passing style) — the caller passes a lambda that receives the result:

```scheme
(http-get "https://example.com/api"
  (lambda (response)
    (display (http-response-status response))
    (display (http-response-body response))))
```

This works on every host:

- **wasmtime**: The runtime does the HTTP request, calls the callback immediately (synchronous from the program's perspective). `pollable.block()` under the hood.
- **Browser JS**: The host starts `fetch()`, returns to the JS event loop, and calls back into WASM when the response arrives.

The compiler doesn't need any async primitives. It's just a function call with a closure argument.

### How callbacks work at the ABI level

Scheme closures are GC structs: `(ref $code, ref $env)`. The host can't call these directly. Instead:

1. `(http-get url callback)` calls an imported function, passing the URL (as ptr+len) and storing the callback closure in a global or table slot
2. The import returns immediately (on the web) or blocks-then-returns (on wasmtime)
3. When the response is ready, the host calls an exported trampoline: `_deliver_http_response(slot, status, headers_ptr, body_ptr, body_len)`
4. The trampoline retrieves the stored closure and applies it with a Scheme response record

On wasmtime, steps 2-3 happen synchronously within the same call. On the web, step 3 happens later via the JS event loop.

### Composition

Sequential requests (nested callbacks):
```scheme
(http-get "https://a.com" (lambda (r1)
  (http-get "https://b.com" (lambda (r2)
    (display (http-response-body r1))
    (display (http-response-body r2))))))
```

Parallel requests:
```scheme
(let ((results '())
      (count 0))
  (define (on-done response)
    (set! results (cons response results))
    (set! count (+ count 1))
    (when (= count 2)
      (for-each (lambda (r) (display (http-response-body r))) results)))
  (http-get "https://a.com" on-done)
  (http-get "https://b.com" on-done))
```

No promises, no async/await, no continuations. Just lambdas.

### Blocking convenience wrapper

For wasmtime-only code (CLI tools, scripts), a blocking wrapper is trivial:

```scheme
;; Could be provided as a library
(define (http-get-sync url)
  (let ((result #f))
    (http-get url (lambda (r) (set! result r)))
    result))
```

On wasmtime this works because the callback fires synchronously. On the web it would return `#f` — user beware. The callback API is the primary interface.

## Scheme API Design

### Client (outgoing handler) — primary focus

```scheme
;; Simple GET with callback
(http-get "https://example.com/api"
  (lambda (response)
    (display (http-response-status response))   ; → 200
    (display (http-response-body response))))    ; → string

;; Full control
(http-request "POST" "https://api.example.com/data"
  '(("content-type" . "application/json"))  ; headers
  "{\"key\": \"value\"}"                    ; body
  (lambda (response)
    (display (http-response-status response))))
```

### Server (incoming handler) — future work

```scheme
;; User defines a handler — the compiler exports it as incoming-handler.handle
(define-http-handler (request respond)
  (let ((method (http-request-method request))
        (path   (http-request-path request))
        (headers (http-request-headers request)))
    (respond
      (make-http-response
        200
        '(("content-type" . "text/html"))
        "<h1>Hello from Puppy Scheme</h1>"))))
```

`define-http-handler` is a macro that:
1. Registers the function as the incoming-handler export
2. At the ABI level, receives `(incoming-request, response-outparam)`, calls the user function, writes the response

### Shared Concepts

**Headers** are alists of `(name . value)` pairs. Names are strings, values are strings (the runtime handles the bytes↔string conversion since HTTP header values are effectively ASCII).

**Bodies** can be:
- A string (buffered, simple case)
- A port (streaming, for large bodies — reuses existing port infrastructure)

**Responses** are records: `(status headers body)`.

### Host Mapping

| Scheme API | WASI (Component Model) | Browser (JS host) |
|---|---|---|
| `http-get` / `http-request` | Import `wasi:http/outgoing-handler.handle`, block on pollable, call callback | JS import starts `fetch()`, calls `_deliver_http_response` trampoline on completion |
| `http-request-method` | `incoming-request.method()` | Read from marshalled request record |
| `http-request-path` | `incoming-request.path-with-query()` | Read from marshalled request record |
| `http-request-headers` | `incoming-request.headers().entries()` | Read from marshalled request record |
| `http-request-body` | `incoming-request.consume()` → stream → port | Read from marshalled request record |
| `make-http-response` | Construct `outgoing-response` + write body + `response-outparam.set` | Return marshalled response to JS |
| `http-response-status` | `incoming-response.status()` | Read from marshalled response record |
| `http-response-body` | `incoming-response.consume()` → stream → read all | Read from marshalled response record |

## Prerequisites

Several compiler features are needed before HTTP can work:

### 1. String marshalling (cabi_realloc)

HTTP is string-heavy (URLs, headers, bodies). The canonical ABI passes strings as `(ptr, len)` in linear memory. The core module must export `memory` and `cabi_realloc` so the host can write strings into the module's memory.

Currently, `wrap-wasi-component` uses a separate helper module for memory/realloc. For HTTP, the core module itself needs these exports because it must read string data the host writes.

**Needed:** `cabi_realloc` as an exported function from the core module. A bump-pointer allocator in linear memory is sufficient (component instances are short-lived).

### 2. Resource handle support

Every HTTP type is a resource. At the core level, resources are `i32` handles. The compiler needs:
- Codegen for `own<T>` and `borrow<T>` — just i32 at the core ABI level
- `resource.drop` calls for cleanup
- Constructor calls (e.g., `fields.constructor()`, `outgoing-response.constructor(headers)`)
- Method calls (e.g., `request.method()`, `body.write()`)

**At the core ABI level, this is just i32 function calls.** The component model's `canon resource.drop`, `canon resource.rep`, etc. handle the high-level semantics. The core module just passes i32 handles around.

### 3. Variant/option/result codegen

The canonical ABI for `option<T>` is `(i32-discriminant, T-flat...)`. For `result<T, E>` it's `(i32-discriminant, max-flat(T, E)...)`. The compiler needs to marshal these between Scheme values and flat ABI representation.

### 4. List codegen

`fields.entries()` returns `list<tuple<string, list<u8>>>`. Lists are `(ptr, len)` in linear memory, with elements laid out contiguously. The compiler needs to read/write lists from/to linear memory.

### 5. Interface-level imports/exports

The current `wrap-component` handles bare function imports/exports. HTTP requires interface-level imports/exports:
```
import wasi:http/outgoing-handler@0.2.0;
export wasi:http/incoming-handler@0.2.0;
```

The component binary encoder needs to emit component type sections for full interface types (with resource type definitions) and match them against imports/exports.

## Implementation Plan

### Phase 1: Core ABI foundations

Build the missing pieces that ALL component model interop needs, not just HTTP:

1. **Export `memory` and `cabi_realloc` from core module** when the WIT world uses string/list types. The WASI P2 auto-wrap can keep its separate helper module (simpler, no user code touches linear memory).

2. **String marshalling.** Read `(ptr, len)` from linear memory into a GC string. Write GC string to linear memory as `(ptr, len)`. Use the same linear memory the module already has for I/O.

3. **Resource handles.** Map `own<T>` / `borrow<T>` to i32 in codegen. Emit `resource.drop` lowerings. At the Scheme level, resources are opaque i31-wrapped handles.

4. **Variant/option/result ABI.** Encode/decode the discriminant+payload pattern. Map to Scheme: `option<T>` → value or `#f`, `result<T, E>` → value or error.

### Phase 2: HTTP client (outgoing-handler)

Start with the simpler direction — making HTTP requests:

1. **Import `wasi:http/outgoing-handler`** in the component envelope.

2. **Implement `http-get` / `http-request`** as builtins with callback API:
   - User calls `(http-get url callback)`
   - Runtime constructs `outgoing-request`, sends via `outgoing-handler.handle`
   - On wasmtime: blocks on `pollable.block()`, reads response, calls callback immediately
   - Callback receives a Scheme response record (status, headers alist, body string)

3. **Callback delivery trampoline.** Export `_deliver_http_response` so hosts can call back into WASM asynchronously. The trampoline retrieves the stored closure and applies it with the response.

4. **Browser host wiring.** A JS host provides the `outgoing-handler` import by calling `fetch()` and invoking the trampoline on completion:
   ```js
   // The host provides this when instantiating the core module
   env: {
     http_request(url_ptr, url_len, method, callback_slot) {
       const url = readString(memory, url_ptr, url_len);
       fetch(url).then(async (resp) => {
         const body = await resp.text();
         writeString(memory, body_ptr, body);
         instance.exports._deliver_http_response(
           callback_slot, resp.status, body_ptr, body.length);
       });
     }
   }
   ```

5. **Test** with a simple GET request against httpbin or similar.

### Phase 3: HTTP server (incoming-handler)

The more complex direction — handling incoming requests:

1. **Export `wasi:http/incoming-handler`** from the component.

2. **`define-http-handler` macro** desugars to a function + registration:
   - The exported `handle(request, response-out)` calls the user function
   - Request accessors read from resource handles
   - Response construction writes to resource handles + sets response-outparam

3. **Streaming bodies** — map `incoming-body.%stream()` to a Scheme input port, `outgoing-body.write()` to a Scheme output port. Reuse the existing port infrastructure (same 5-field struct: fd→handle, mode, buf, src-str, pos).

4. **Browser host wiring.** Works with Cloudflare Workers, Deno Deploy, or any Service Worker environment — the JS host marshals the `Request` object into WASM memory, calls the exported handler, and marshals the response back into a `Response` object.

### Phase 4: Full proxy world

Wire up the complete `wasi:http/proxy` world:
- Both incoming-handler export and outgoing-handler import
- Forward proxy pattern: receive request, make outbound request, return response
- Concurrent streaming via nested callbacks

## Size Considerations

The component envelope for HTTP is larger than for CLI because of the interface types (resources, variants, lists). Rough estimate:

| Component | Envelope overhead |
|---|---|
| WASI CLI (current) | ~800 bytes |
| WASI HTTP proxy | ~2-4 KB (many resource types, error-code variant has 30+ cases) |
| User code | Unchanged |

For a "Hello World" HTTP server, total binary might be 5-10 KB. Still well within the "small output" goal.

## Open Questions

1. **Body streaming granularity.** Should `http-response-body` always read the entire body into a string, or should we support incremental reading via ports? Ports are more general but require the full port infrastructure for every HTTP program. Start with buffered strings, add port-based streaming later.

2. **Error handling.** The `error-code` variant has 30+ cases. Exposing all of them to Scheme is noisy. Options: (a) map to a small set of Scheme error symbols, (b) provide the full error as an alist, (c) just use `(error "HTTP error: ...")` for now.

3. **Header value encoding.** WASI HTTP uses `list<u8>` for header values (because HTTP allows non-UTF-8 bytes). Scheme strings are byte arrays internally, so this maps naturally — but we need list↔bytevector conversion at the ABI boundary.

4. **Callback storage.** How many in-flight requests can we support? A fixed-size table (e.g., 16 slots) is simple. A growable list is more general. For Phase 2, a single global slot (one request at a time) might be enough to start.

5. **Timeout support.** `request-options` allows setting connect/first-byte/between-bytes timeouts. Expose as optional keyword-style arguments to `http-request`, or as a separate `make-request-options` constructor?
