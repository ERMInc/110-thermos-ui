{pkgs, config, ...} :
let
  pg = pkgs.postgresql;
  pgis = pkgs.postgis.override { postgresql = pg; };
  create-database = builtins.toFile "create-database.sql"
  ''
    CREATE ROLE postgres LOGIN;
    GRANT root TO postgres;
    ALTER USER postgres WITH PASSWORD 'therm0s';
    CREATE DATABASE thermos;
  '';
  enable-postgis = builtins.toFile "enable-postgis.sql"
  "CREATE EXTENSION postgis; CREATE EXTENSION postgis_raster;";
  scip = (pkgs.callPackage ./scip.nix {});
in
{
  environment.systemPackages = [
    pkgs.rxvt_unicode.terminfo
    pkgs.xclip
    pkgs.xorg.xauth

    (pkgs.stdenv.mkDerivation {
      name="get-lidar";
      src = ./get-lidar.sh;
      unpackPhase = ''
        true
      '';
      buildInputs = [pkgs.makeWrapper];
      installPhase = ''
        mkdir -p $out/bin
        makeWrapper $src $out/bin/get-lidar --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.gdal pkgs.jq pkgs.curl pkgs.findutils pkgs.unzip]}
      '';
    })
  ];

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.openssh.forwardX11 = true;

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

      location / {
      proxy_pass http://localhost:8080;
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;

      proxy_read_timeout 7200;
      proxy_send_timeout 7200;
      send_timeout 7200;
      }
      }
    '';
  };

  services.postgresql = {
    enable = true;
    package = pg;
    extraPlugins = [ pgis ];
    enableTCPIP = true;

    authentication = ''
      local all all trust
      host all all all trust
    '';

    initialScript = create-database;
  };

  systemd.services.postgresql.environment.POSTGIS_GDAL_ENABLED_DRIVERS="ENABLE_ALL";

  systemd.services.enable-postgis = {
    path = [pg];
    wantedBy = ["multi-user.target"];
    after = ["postgresql.service"];
    requires = ["postgresql.service"];
    script = ''
      [[ -f /var/lib/enabled-postgis ]] ||
      ( psql -U postgres -d thermos -a -f "${enable-postgis}" &&
      touch /var/lib/enabled-postgis )
    '';
  };

  systemd.services.thermos-solver = {
    serviceConfig = {
      TimeoutStopSec = "60s";
    };
    wantedBy = ["multi-user.target"];
    after = ["enable-postgis.service"];
    requires = ["enable-postgis.service"];
    path = [scip];
    script = ''
      export PG_HOST=127.0.0.1
      export LIDAR_DIRECTORY=/thermos-lidar/

      export SOLVER_COUNT=16
      export SMTP_ENABLED=false
      export WEB_SERVER_ENABLED=false
      export IMPORTER_COUNT=0

      exec ${pkgs.jre}/bin/java -Xmx4g -server -jar ${../target/thermos.jar}
    '';
  };

  systemd.services.thermos-importer = {
    serviceConfig = {
      TimeoutStopSec = "60s";
    };
    wantedBy = ["multi-user.target"];
    after = ["enable-postgis.service"];
    requires = ["enable-postgis.service"];

    script = ''
      export PG_HOST=127.0.0.1
      export LIDAR_DIRECTORY=/thermos-lidar/

      export SOLVER_COUNT=0
      export SMTP_ENABLED=false
      export WEB_SERVER_ENABLED=false
      export IMPORTER_COUNT=4

      exec ${pkgs.jre}/bin/java -Xmx4g -server -jar ${../target/thermos.jar}
    '';
  };
  
  systemd.services.thermos-web = {
    serviceConfig = {
      TimeoutStopSec = "60s";
    };
    wantedBy = ["multi-user.target"];
    after = ["enable-postgis.service"];
    requires = ["enable-postgis.service"];
    script =
      ''
        export PG_HOST=127.0.0.1
        export LIDAR_DIRECTORY=/thermos-lidar/

        export SOLVER_COUNT=0
        export IMPORTER_COUNT=0
        export SMTP_ENABLED=true
        export WEB_SERVER_ENABLED=true

        export SMTP_HOST=smtp.hosts.co.uk
        export SMTP_PORT=25
        export SMTP_TLS=true
        export SMTP_USER=thermos-project.eu
        while [[ ! -f /run/keys/smtp ]]; do 
        echo "waiting for smtp key"
        sleep 2
        done
        export SMTP_PASSWORD=$(cat /run/keys/smtp)
        export SMTP_FROM_ADDRESS="THERMOS <system@thermos-project.eu>"
        export BASE_URL="https://tool.thermos-project.eu"

        export WEB_SERVER_DISABLE_CACHE=false

        exec ${pkgs.jre}/bin/java -Xmx6g -server -jar ${../target/thermos.jar}
      '';
  };

  security.sudo.wheelNeedsPassword = false;
  
  users.motd = ''
    THERMOS demo server
    -------------------
    LIDAR is stored in /thermos-lidar/
  '';

  users.users.root.initialPassword = "hello";
  
  users.users.josh = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys =
      [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDRkGcgihmDtnBlBlfuBMr61ShKhuGAPSptHpna5nTk/XgGtjAwBMo4trnCaXo2w3SOYwV6oIOV1gHy8Bsg/96zbpXPUHMAjMQ32p18bZCDsgCQhz5LroWAl2zz2MfEuQc35Wiy6zjFhAPQ6WbRfpiWGW97KZ0y0XG7XLwRj4J731CQN8tJBaPjQkKG4PPc1/mHfNjJ4l+v88lo6WZGHpInhOjVZjtYeYyVTHCFVZNPNDg5RM7nXvuMCqdzKnb6a5YKS1CcPKkQKG1zElSzA+kzC2QowOAjVwghm5+3Y3+826HElAdoGa7c0IGmv6PC4aTNHyjnHvUB0CZ+YIHn4YnGxnDWQBJRRKbjM5M99MtHY3cvYPR3TucJ+6XQ4ZLHsBSu0XZ8Uj4op1E3A2mie7Xw8k34A/mYig3E4AHa06M8libnsTkHq+Z0gCDM2zfq+3QIVse/e7M/PTdMhMq2FvbCiJ7HP4HyDYXeMNR35rPOi1GLopetqwFa/7ShPIWpq6y+Jh7PnCel+fSBxbaCsjuAqfJOrF88lwtZHszf0Sn4g3iXbdsdFRCqIitUpHcqL7RPJNJy0hFkhCT38lvvZ/dedrWX4gSBMSwyMt+yKvIaOsTVak5CihahKqfEDZZ7D4N1rrplITMFFtf5c93/dkWWU4RKZvzf9T8PJGovhW+ppQ==" ]
      ;
  };
}