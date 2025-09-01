(ns app.server
  (:require
   [org.httpkit.server :as http]
   [clojure.string :as str]
   [hiccup2.core :as h]
   [ansi.hiccup.stream :as ahs])
  (:gen-class))

(defonce !state (atom (ahs/new-state)))
(defonce !server (atom nil))

(defn- html-page []
  (str
   "<!doctype html><html><head><meta charset='utf-8'/>"
   "<meta name='viewport' content='width=device-width, initial-scale=1'/>"
   "<title>ANSI → Hiccup Stream</title>"
   "<style>"
   "* { box-sizing: border-box; }"
   "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Inter,sans-serif;margin:0;padding:0;background:#0b0d10;color:#e6edf3}"
   "header{padding:16px 20px;border-bottom:1px solid #1f232a;background:#0f1319}"
   "main{display:grid;grid-template-columns: 420px 1fr; gap: 20px; padding: 20px; max-width: 100vw; overflow-x: auto;}"
   "textarea{width:100%;height:120px;background:#0f1319;color:#e6edf3;border:1px solid #2c313a;border-radius:8px;padding:10px;font-family:ui-monospace, SFMono-Regular, Menlo, monospace;resize:vertical;min-height:80px;}"
   "button{margin:4px 8px 4px 0;padding:10px 16px;border-radius:8px;border:1px solid #2c313a;background:#151b23;color:#e6edf3;cursor:pointer;border:none;font-size:14px;}"
   "button:hover{background:#1a2130}"
   ".button-group{margin:16px 0;}"
   ".panel{background:#0f1319;border:1px solid #2c313a;border-radius:10px;padding:20px;min-width:0;overflow:hidden;}"
   ".panel h4{margin-top:0;margin-bottom:16px;color:#e6edf3;}"
   ".panel p{margin:16px 0;line-height:1.5;word-wrap:break-word;overflow-wrap:break-word;}"
   "code{background:#21262d;color:#f0f6fc;padding:2px 6px;border-radius:4px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:0.9em;word-break:break-all;}"
   ".terminal{white-space:pre-wrap;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;word-break:break-all;padding:16px;background:#0d1117;border-radius:6px;border:1px solid #21262d;}"
   ".b{font-weight:700}.i{font-style:italic}.u{text-decoration:underline}"
   ".fg-black{color:#000}.fg-red{color:#a00}.fg-green{color:#0a0}.fg-yellow{color:#aa0}"
   ".fg-blue{color:#06c}.fg-magenta{color:#a0a}.fg-cyan{color:#0aa}.fg-white{color:#ddd}"
   ".fg-bright-red{color:#e00}.bg-red{background:#a002}.t-link{text-decoration:underline;color:#3aa6ff}"
   "@media (max-width: 768px) { main { grid-template-columns: 1fr; gap: 16px; } }"
   "</style></head><body>"
   "<header><h3>ANSI → Hiccup Streaming Demo</h3></header>"
   "<main>"
   "<section class='panel'>"
   "<h4>Send Chunk</h4>"
   "<textarea id='chunk' placeholder='Type some text with ANSI escapes, e.g. \\u001b[31mRED\\u001b[0m'></textarea>"
   "<div class='button-group'>"
   "<button onclick='sendChunk()'>Send Chunk</button>"
   "<button onclick='loadDemo()'>Load Demo</button>"
   "<button onclick='resetStyles()'>Reset Styles</button>"
   "<button onclick='clearOut()'>Clear Output</button>"
   "</div>"
   "<p style='opacity:.8'>Tip: Try <code>\\u001b[1mbold\\u001b[0m</code>, "
   "<code>\\u001b[34mblue\\u001b[0m</code>, or an OSC-8 link: "
   "<code>\\u001b]8;;https://example.com\\u0007click\\u001b]8;;\\u0007</code></p>"
   "</section>"
   "<section class='panel'>"
   "<h4>Rendered Output</h4>"
   "<div id='out' class='terminal' style='min-height:280px'></div>"
   "</section>"
   "</main>"
   "<script>"
   "const sleep = (ms)=>new Promise(r=>setTimeout(r,ms));"
   "async function fetchHtml(){"
   "const r = await fetch('/html',{cache:'no-store'});"
   "document.getElementById('out').innerHTML = await r.text();"
   "}"
   "async function sendChunk(){"
   "const raw = document.getElementById('chunk').value;"
   "const chunk = JSON.parse('\"'+raw.replace(/\\\\/g,'\\\\\\\\').replace(/\"/g,'\\\\\"')+'\"');"
   "await fetch('/ingest',{method:'POST',headers:{'content-type':'text/plain'},body:chunk});"
   "document.getElementById('chunk').value='';"
   "await fetchHtml();"
   "}"
   "async function loadDemo(){ await fetch('/demo',{method:'POST'}); await fetchHtml(); }"
   "async function resetStyles(){ await fetch('/reset',{method:'POST'}); await fetchHtml(); }"
   "async function clearOut(){ await fetch('/clear',{method:'POST'}); await fetchHtml(); }"
   "(async()=>{ while(true){ await fetchHtml(); await sleep(400);} })();"
   "</script>"
   "</body></html>"))

(defn- respond [status body & [headers]]
  {:status status
   :headers (merge {"content-type" "text/html; charset=utf-8"} headers)
   :body body})

(defn- to-html []
  (let [hiccup (ahs/state->hiccup @!state)]
    (str (h/html hiccup))))

(defn handler [req]
  (let [uri (:uri req)
        meth (:request-method req)]
    (cond
      (and (= uri "/") (= meth :get))
      (respond 200 (html-page))

      (and (= uri "/html") (= meth :get))
      (respond 200 (to-html) {"content-type" "text/plain; charset=utf-8"})

      (and (= uri "/ingest") (= meth :post))
      (let [body-stream (:body req)
            body (when body-stream (slurp body-stream))
            q (:query-string req)
            chunk (if (and (seq q) (str/starts-with? q "chunk="))
                    (java.net.URLDecoder/decode (subs q 6) "UTF-8")
                    (or body ""))]
        (swap! !state ahs/ingest chunk)
        (respond 204 ""))

      (and (= uri "/reset") (= meth :post))
      (do (swap! !state ahs/reset-styles)
          (respond 204 ""))

      (and (= uri "/clear") (= meth :post))
      (do (swap! !state ahs/clear)
          (respond 204 ""))

      (and (= uri "/demo") (= meth :post))
      (do (swap! !state ahs/ingest ahs/demo)
          (respond 204 ""))

      :else (respond 404 "<h1>Not found</h1>"))))

(defn -main [& _]
  (when-let [s @!server] (s))
  (reset! !state (ahs/new-state))
  (println "Starting server on http://localhost:8080")
  (reset! !server (http/run-server handler {:port 8080}))
  @(promise))
