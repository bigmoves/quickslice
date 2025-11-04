/// Upload interface handler
///
/// Serves a simple form to upload blobs and test the uploadBlob mutation
import envoy
import gleam/http
import wisp

pub fn handle_upload_request(req: wisp.Request) -> wisp.Response {
  case req.method {
    http.Get -> handle_upload_form()
    http.Post -> handle_upload_submit(req)
    _ -> method_not_allowed_response()
  }
}

fn handle_upload_form() -> wisp.Response {
  // Read optional OAuth token from environment for testing
  let oauth_token = case envoy.get("GRAPHIQL_OAUTH_TOKEN") {
    Ok(token) -> token
    Error(_) -> ""
  }

  let upload_html = "<!doctype html>
<html lang=\"en\">
  <head>
    <meta charset=\"UTF-8\" />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
    <title>QuickSlice - Upload Blob</title>
    <script src=\"https://cdn.tailwindcss.com\"></script>
  </head>
  <body class=\"bg-gray-50 min-h-screen p-8\">
    <div class=\"max-w-2xl mx-auto\">
      <div class=\"mb-8\">
        <h1 class=\"text-4xl font-bold text-gray-900 mb-2\">Upload Blob</h1>
        <p class=\"text-gray-600\">Test the uploadBlob mutation by uploading a file</p>
      </div>

      <div class=\"bg-white rounded-lg shadow-sm border border-gray-200 p-6\">
        <form id=\"uploadForm\" class=\"space-y-6\">
          <div>
            <label for=\"token\" class=\"block text-sm font-medium text-gray-700 mb-2\">
              OAuth Token
            </label>
            <input
              type=\"text\"
              id=\"token\"
              name=\"token\"
              value=\"" <> oauth_token <> "\"
              class=\"w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-purple-500 focus:border-purple-500\"
              placeholder=\"Enter your OAuth token\"
              required
            />
            <p class=\"mt-1 text-sm text-gray-500\">
              Get this from your GRAPHIQL_OAUTH_TOKEN environment variable
            </p>
          </div>

          <div>
            <label for=\"file\" class=\"block text-sm font-medium text-gray-700 mb-2\">
              File to Upload
            </label>
            <input
              type=\"file\"
              id=\"file\"
              name=\"file\"
              class=\"w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-purple-500 focus:border-purple-500\"
              required
            />
            <p class=\"mt-1 text-sm text-gray-500\">
              Select an image or any file to upload as a blob
            </p>
          </div>

          <div>
            <button
              type=\"submit\"
              class=\"w-full bg-purple-600 hover:bg-purple-700 text-white font-semibold py-3 px-4 rounded-lg transition-colors shadow-sm\"
            >
              Upload Blob
            </button>
          </div>
        </form>

        <div id=\"result\" class=\"mt-6 hidden\">
          <h2 class=\"text-lg font-semibold text-gray-900 mb-2\">Result:</h2>
          <div id=\"resultContent\" class=\"bg-gray-50 rounded-lg p-4 border border-gray-200 font-mono text-sm overflow-auto\"></div>
        </div>

        <div id=\"error\" class=\"mt-6 hidden\">
          <h2 class=\"text-lg font-semibold text-red-900 mb-2\">Error:</h2>
          <div id=\"errorContent\" class=\"bg-red-50 rounded-lg p-4 border border-red-200 text-red-700 font-mono text-sm overflow-auto\"></div>
        </div>
      </div>
    </div>

    <script>
      const form = document.getElementById('uploadForm');
      const fileInput = document.getElementById('file');
      const tokenInput = document.getElementById('token');
      const resultDiv = document.getElementById('result');
      const resultContent = document.getElementById('resultContent');
      const errorDiv = document.getElementById('error');
      const errorContent = document.getElementById('errorContent');

      form.addEventListener('submit', async (e) => {
        e.preventDefault();

        // Hide previous results
        resultDiv.classList.add('hidden');
        errorDiv.classList.add('hidden');

        const file = fileInput.files[0];
        const token = tokenInput.value;

        if (!file || !token) {
          showError('Please select a file and provide a token');
          return;
        }

        try {
          // Read file as ArrayBuffer and convert to base64
          const arrayBuffer = await file.arrayBuffer();
          const bytes = new Uint8Array(arrayBuffer);
          let binary = '';
          for (let i = 0; i < bytes.byteLength; i++) {
            binary += String.fromCharCode(bytes[i]);
          }
          const base64Data = btoa(binary);

          // Call our GraphQL uploadBlob mutation
          const mutation = `
            mutation UploadBlob($data: String!, $mimeType: String!) {
              uploadBlob(data: $data, mimeType: $mimeType) {
                ref
                mimeType
                size
              }
            }
          `;

          const graphqlResponse = await fetch('/graphql', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              'Authorization': `Bearer ${token}`
            },
            body: JSON.stringify({
              query: mutation,
              variables: {
                data: base64Data,
                mimeType: file.type || 'application/octet-stream'
              }
            })
          });

          const graphqlData = await graphqlResponse.json();

          if (graphqlData.errors && graphqlData.errors.length > 0) {
            throw new Error(JSON.stringify(graphqlData.errors, null, 2));
          }

          showResult({
            file: {
              name: file.name,
              size: file.size,
              type: file.type
            },
            graphqlResponse: graphqlData
          });
        } catch (error) {
          showError(error.message);
        }
      });

      function showResult(data) {
        resultContent.textContent = JSON.stringify(data, null, 2);
        resultDiv.classList.remove('hidden');
      }

      function showError(message) {
        errorContent.textContent = message;
        errorDiv.classList.remove('hidden');
      }
    </script>
  </body>
</html>"

  wisp.response(200)
  |> wisp.set_header("content-type", "text/html; charset=utf-8")
  |> wisp.set_body(wisp.Text(upload_html))
}

fn handle_upload_submit(_req: wisp.Request) -> wisp.Response {
  // For now, we'll handle uploads via JavaScript on the client side
  // If we need server-side processing, we can implement it here
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"Not implemented\", \"message\": \"Use client-side upload\"}",
  ))
}

fn method_not_allowed_response() -> wisp.Response {
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"MethodNotAllowed\", \"message\": \"Only GET is allowed\"}",
  ))
}
