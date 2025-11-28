/// GraphiQL interface handler
///
/// Serves the GraphiQL interactive GraphQL IDE
import admin_session as session
import gleam/erlang/process.{type Subject}
import lib/oauth/did_cache
import sqlight
import wisp

pub fn handle_graphiql_request(
  req: wisp.Request,
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
) -> wisp.Response {
  // Get token from session if logged in
  let oauth_token = case session.get_current_user(req, db, did_cache) {
    Ok(#(_did, _handle, access_token)) -> access_token
    Error(_) -> ""
  }
  let graphiql_html = "<!doctype html>
<html lang=\"en\">
  <head>
    <meta charset=\"UTF-8\" />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
    <title>quickslice GraphiQL</title>
    <style>
      body {
        margin: 0;
      }

      #graphiql {
        height: 100dvh;
      }

      .loading {
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 4rem;
      }
    </style>
    <link rel=\"stylesheet\" href=\"https://esm.sh/graphiql/dist/style.css\" />
    <link
      rel=\"stylesheet\"
      href=\"https://esm.sh/@graphiql/plugin-explorer/dist/style.css\"
    />
    <script type=\"importmap\">
      {
        \"imports\": {
          \"react\": \"https://esm.sh/react@19.1.0\",
          \"react/\": \"https://esm.sh/react@19.1.0/\",

          \"react-dom\": \"https://esm.sh/react-dom@19.1.0\",
          \"react-dom/\": \"https://esm.sh/react-dom@19.1.0/\",

          \"graphiql\": \"https://esm.sh/graphiql@5.2.1?standalone&external=react,react-dom,@graphiql/react,graphql,monaco-editor,monaco-graphql\",
          \"graphiql/\": \"https://esm.sh/graphiql@5.2.1/\",
          \"@graphiql/plugin-explorer\": \"https://esm.sh/@graphiql/plugin-explorer?standalone&external=react,@graphiql/react,graphql\",
          \"@graphiql/react\": \"https://esm.sh/@graphiql/react@0.37.2?standalone&external=react,react-dom,graphql,@graphiql/toolkit,@emotion/is-prop-valid,monaco-editor,monaco-graphql\",

          \"@graphiql/toolkit\": \"https://esm.sh/@graphiql/toolkit?standalone&external=graphql\",
          \"graphql\": \"https://esm.sh/graphql@16.11.0\",
          \"@emotion/is-prop-valid\": \"data:text/javascript,\",

          \"monaco-editor\": \"https://esm.sh/monaco-editor@0.52.2\",
          \"monaco-editor/\": \"https://esm.sh/monaco-editor@0.52.2/\",
          \"monaco-graphql\": \"https://esm.sh/monaco-graphql@1.6.0?deps=monaco-editor@0.52.2,graphql@16.11.0\",
          \"monaco-graphql/\": \"https://esm.sh/monaco-graphql@1.6.0/\"
        }
      }
    </script>
    <script type=\"module\">
      import React from 'react';
      import ReactDOM from 'react-dom/client';
      import { GraphiQL, HISTORY_PLUGIN } from 'graphiql';
      import { createGraphiQLFetcher } from '@graphiql/toolkit';
      import { explorerPlugin } from '@graphiql/plugin-explorer';

      // Manual worker setup - graphiql/setup-workers/esm.sh has compatibility issues
      // Pin to monaco-editor@0.52.2 per https://github.com/graphql/graphiql/issues/4104
      import JsonWorker from 'https://esm.sh/monaco-editor@0.52.2/esm/vs/language/json/json.worker.js?worker';
      import GraphQLWorker from 'https://esm.sh/monaco-graphql@1.6.0/esm/graphql.worker.js?worker&external=monaco-editor&deps=graphql@16.11.0';
      import EditorWorker from 'https://esm.sh/monaco-editor@0.52.2/esm/vs/editor/editor.worker.js?worker';

      globalThis.MonacoEnvironment = {
        getWorker(_workerId, label) {
          if (label === 'json') return new JsonWorker();
          if (label === 'graphql') return new GraphQLWorker();
          return new EditorWorker();
        },
      };

      const token = '" <> oauth_token <> "';
      const fetcher = createGraphiQLFetcher({
        url: '/graphql',
        subscriptionUrl: window.location.protocol === 'https:' ? 'wss://' + window.location.host + '/graphql' : 'ws://' + window.location.host + '/graphql',
        headers: token ? {
          'Authorization': token
        } : {}
      });
      const plugins = [HISTORY_PLUGIN, explorerPlugin()];

      function App() {
        return React.createElement(GraphiQL, {
          fetcher,
          plugins,
          defaultEditorToolsVisibility: true,
        });
      }

      const container = document.getElementById('graphiql');
      const root = ReactDOM.createRoot(container);
      root.render(React.createElement(App));
    </script>
  </head>
  <body>
    <div id=\"graphiql\">
      <div class=\"loading\">Loading…</div>
    </div>
  </body>
</html>"

  wisp.response(200)
  |> wisp.set_header("content-type", "text/html; charset=utf-8")
  |> wisp.set_body(wisp.Text(graphiql_html))
}
