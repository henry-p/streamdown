# ANSI → Hiccup Streaming Demo

This is a minimal Clojure project that **streams ANSI/OSC output into a continuously well‑formed Hiccup tree**, and serves it over HTTP.

## Requirements
- Java 17+
- Clojure CLI (`clj` / `clojure`)

## Run

```bash
clj -M -m app.server
```

Server starts on **http://localhost:8080**.

Open the page and:
- Click **Load Demo** to ingest a demo ANSI sequence.
- Type in the **Chunk** textarea and click **Send Chunk** to live-ingest your own ANSI (e.g. `\u001b[31mRED\u001b[0m`).
- Click **Reset Styles** to clear SGR flags (keeps content).
- Click **Clear Output** to clear all collected segments.

The viewer polls `/html` every ~400ms to update the rendering. The underlying Hiccup is *always valid* no matter where the stream was cut.

## Endpoints

- `GET /` → static UI
- `GET /html` → current HTML (rendered from Hiccup)
- `POST /ingest` (body = raw text/bytes; or `?chunk=...`) → appends to stream
- `POST /reset` → reset SGR styles (keeps segments)
- `POST /clear` → clear segments (keeps styles)
- `POST /demo` → load built-in demo into state

## Notes

- This demo focuses on **styling semantics** (SGR + OSC 8 hyperlinks, 256-color, truecolor). It **does not** emulate cursor-addressing or a cell grid (no overwrite, no CR/LF handling beyond text).
- If you need cursor/overwriting TUIs, add a higher-level line/grid model on top and feed formatted rows into `ansi.hiccup.stream/ingest`.

