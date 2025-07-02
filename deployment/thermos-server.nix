{pkgs, config, ...} :
let
  scip = (pkgs.callPackage ./scip.nix {});
  gurobi = (pkgs.callPackage ./gurobi.nix {});
in
{
  imports = [ ./thermos.nix ];

  networking.firewall.allowedTCPPorts = [ 80 ];

  nixpkgs.config.allowUnfree = true;

  services.thermos.ui.enable = true;
  services.thermos.model.enable = true;
  services.thermos.importer.enable = true;
  environment.systemPackages = [
    pkgs.vim
    pkgs.ranger
    pkgs.cron
    gurobi
  ];

  services.cron = {
    enable = true;
  };
  
  services.thermos.ui.baseUrl = "http://ec2-13-41-145-247.eu-west-2.compute.amazonaws.com";

  services.nginx = {
    enable = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    virtualHosts = {};
    appendHttpConfig = ''
      # GZIP more MIME types than in the recommended gzip settings
      gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript application/x-javascript image/svg+xml text/html application/xhtml+xml;

      add_header Strict-Transport-Security max-age=15768000;

      server {
           listen 80 default_server;

           client_max_body_size 1000M;

           gzip on;
           gzip_proxied any;
           gzip_types text/css text/javascript application/json text/plain text/xml application/javascript application/octet-stream;
           error_page 403 404 500 502 503 504 /error.html;

           location /error.html {
             internal;
             root ${./error};
           }

           location / {
             proxy_pass http://localhost:${toString config.services.thermos.ui.port}/;
             proxy_set_header Host $host;
             proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
             proxy_read_timeout 7200;
             proxy_send_timeout 7200;
             send_timeout 7200;
           }

      }
    '';
  };

  security.sudo.wheelNeedsPassword = false;

  users.motd = ''
    THERMOS demo server
    -------------------
    LIDAR is stored in /thermos-lidar/
  '';
}
