import components/layout
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

/// Render the upload blob page
pub fn view(handle: String, oauth_token: String) -> Element(msg) {
  layout.page(title: "QuickSlice - Upload Blob", content: [
    render_header(handle),
    render_upload_form(oauth_token),
  ])
}

/// Render the page header with user info
fn render_header(handle: String) -> Element(msg) {
  html.div([attribute.class("mb-8 flex justify-between items-center")], [
    html.div([], [
      html.h1([attribute.class("text-4xl font-bold text-gray-900 mb-2")], [
        element.text("Upload Blob"),
      ]),
      html.p([attribute.class("text-gray-600")], [
        element.text("Test the uploadBlob mutation by uploading a file"),
      ]),
    ]),
    html.div([attribute.class("text-right")], [
      html.p([attribute.class("text-sm text-gray-600")], [
        element.text("Logged in as"),
      ]),
      html.p([attribute.class("text-lg font-semibold text-gray-900")], [
        element.text("@" <> handle),
      ]),
      html.a(
        [
          attribute.href("/"),
          attribute.class("text-sm text-purple-600 hover:text-purple-700"),
        ],
        [element.text("← Back to Home")],
      ),
    ]),
  ])
}

/// Render the upload form with JavaScript
fn render_upload_form(oauth_token: String) -> Element(msg) {
  html.div([attribute.class("bg-white rounded-lg shadow-sm border border-gray-200 p-6")], [
    html.form([attribute.id("uploadForm"), attribute.class("space-y-6")], [
      html.input([
        attribute.type_("hidden"),
        attribute.id("token"),
        attribute.value(oauth_token),
      ]),
      html.div([], [
        html.label(
          [
            attribute.for("file"),
            attribute.class("block text-sm font-medium text-gray-700 mb-2"),
          ],
          [element.text("File to Upload")],
        ),
        html.input([
          attribute.type_("file"),
          attribute.id("file"),
          attribute.name("file"),
          attribute.class(
            "w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-purple-500 focus:border-purple-500",
          ),
          attribute.attribute("required", ""),
        ]),
        html.p([attribute.class("mt-1 text-sm text-gray-500")], [
          element.text("Select an image or any file to upload as a blob"),
        ]),
      ]),
      html.div([], [
        html.button(
          [
            attribute.type_("submit"),
            attribute.class(
              "w-full bg-purple-600 hover:bg-purple-700 text-white font-semibold py-3 px-4 rounded-lg transition-colors shadow-sm",
            ),
          ],
          [element.text("Upload Blob")],
        ),
      ]),
    ]),
    html.div([attribute.id("result"), attribute.class("mt-6 hidden")], [
      html.h2([attribute.class("text-lg font-semibold text-gray-900 mb-2")], [
        element.text("Result:"),
      ]),
      html.div([
        attribute.id("resultContent"),
        attribute.class(
          "bg-gray-50 rounded-lg p-4 border border-gray-200 font-mono text-sm overflow-auto",
        ),
      ], []),
    ]),
    html.div([attribute.id("error"), attribute.class("mt-6 hidden")], [
      html.h2([attribute.class("text-lg font-semibold text-red-900 mb-2")], [
        element.text("Error:"),
      ]),
      html.div([
        attribute.id("errorContent"),
        attribute.class(
          "bg-red-50 rounded-lg p-4 border border-red-200 text-red-700 font-mono text-sm overflow-auto",
        ),
      ], []),
    ]),
    render_upload_script(),
  ])
}

/// Render the JavaScript for handling file upload
fn render_upload_script() -> Element(msg) {
  let script_content =
    "
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
    "

  html.script([], script_content)
}
