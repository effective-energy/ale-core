{ pkgs ? (import ../nixpkgs.nix {}) }:

let
  ale-wallet = (import ../. { inherit pkgs; }).ale.wallet;

in rec {
  swagger-ui = pkgs.stdenv.mkDerivation rec {
    pname = "swagger-ui";
    version = "3.9.3";
    name = "${pname}-${version}";

    src = pkgs.fetchurl {
      url = "https://github.com/swagger-api/swagger-ui/archive/v3.9.3.tar.gz";
      sha256 = "1ypdgf7saznqybpcx84iqymgzck3vz6dv5gig5jarmijfyrhkra0";
    };

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p "$out"
      rm dist/*.map
      cp -R dist/* "$out/"
    '';
  };

  wallet-api = pkgs.stdenv.mkDerivation {
    name = "wallet-api-docs";

    phases = [ "buildPhase" "installPhase" ];

    buildPhase = ''
      cat > index.html <<EOF
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <title>Ale Wallet OpenAPI</title>
        <link href="https://fonts.googleapis.com/css?family=Open+Sans:400,700|Source+Code+Pro:300,600|Titillium+Web:400,600,700" rel="stylesheet">
        <link rel="stylesheet" type="text/css" href="./swagger-ui/swagger-ui.css" >
        <link rel="icon" type="image/png" href="./favicon-32x32.png" sizes="32x32" />
        <link rel="icon" type="image/png" href="./favicon-16x16.png" sizes="16x16" />
        <style>
          html
          {
            box-sizing: border-box;
            overflow: -moz-scrollbars-vertical;
            overflow-y: scroll;
          }
          *,
          *:before,
          *:after
          {
            box-sizing: inherit;
          }

          body {
            margin:0;
            background: #fafafa;
          }
        </style>
      </head>

      <body>
      <div id="swagger-ui"></div>

      <script src="./swagger-ui/swagger-ui-bundle.js"> </script>
      <script src="./swagger-ui/swagger-ui-standalone-preset.js"> </script>
      <script>
      function HideTopbarPlugin() {
        return {
          components: {
            Topbar: function() { return null }
          }
        }
      }

      window.onload = function() {
        const ui = SwaggerUIBundle({
          url: "/openapi.json",
          validatorUrl: null,
          dom_id: '#swagger-ui',
          deepLinking: true,
          presets: [
            SwaggerUIBundle.presets.apis,
            SwaggerUIStandalonePreset
          ],
          plugins: [
            SwaggerUIBundle.plugins.DownloadUrl,
            HideTopbarPlugin
          ],
          layout: "StandaloneLayout",
          apisSorter: "alpha",
          operationsSorter: "alpha",
          jsonEditor: true
        })
        window.ui = ui
      }
      </script>
      </body>
      </html>
      EOF
    '';

    installPhase = ''
      mkdir -p "$out"
      cp index.html "$out"

      mkdir -p "$out/swagger-ui"
      cp "${swagger-ui}"/* "$out/swagger-ui"
      mv "$out/swagger-ui/favicon"* "$out"
      rm "$out/swagger-ui/index.html"

    '';
  };

  all = pkgs.buildEnv {
    name = "all-docs";
    paths = [];
    postBuild = ''
      ln -s "${wallet-api}" "$out/openapi"
    '';
  };
}
