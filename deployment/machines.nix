{
  network.description = "THERMOS application";

  thermos-app = {pkgs, config, ...} :
  let
    pg = pkgs.postgresql;
  pgis = pkgs.postgis.override { postgresql = pg; };
  create-database = builtins.toFile "create-database.sql"
  ''
    CREATE ROLE postgres LOGIN;
    GRANT root TO postgres;
    ALTER USER postgres WITH PASSWORD 'therm0s';
    CREATE DATABASE thermos_geometries;
  '';
  enable-postgis = builtins.toFile "enable-postgis.sql"
    "CREATE EXTENSION postgis;";
  in
  {
    environment.systemPackages = [pkgs.rxvt_unicode.terminfo pkgs.xclip pkgs.xorg.xauth];

    networking.firewall.allowedTCPPorts = [ 80 ];

    services.openssh.forwardX11 = true;

    services.nginx = {
      enable = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
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
        location /heat-map-tiles {
            proxy_pass http://localhost:8081;
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

    systemd.services.enable-postgis = {
      path = [pg];
      wantedBy = ["multi-user.target"];
      after = ["postgresql.service"];
      requires = ["postgresql.service"];
      script = ''
        [[ -f /var/lib/enabled-postgis ]] ||
        ( psql -U postgres -d thermos_geometries -a -f "${enable-postgis}" &&
          touch /var/lib/enabled-postgis )
      '';
    };

    systemd.services.tiles =
    {
      wantedBy = ["multi-user.target"];
      after = ["postgresql.service"];
      requires = ["postgresql.service"];

      path = [
      	(pkgs.callPackage ./tileserver-env.nix {})
      ];

      script =
      let
      tileserver = ./density-tile-server;
      in
      ''
      export DB="dbname='thermos_geometries' user='postgres' host='127.0.0.1' password='password'"
      export SCALE="/tile-scale.json"
      export PORT=8081
      python3 ${tileserver}/tileserver.py
      '';
    };

    systemd.services.thermos =
    {
      wantedBy = ["multi-user.target"];
      after = ["enable-postgis.service"];
      requires = ["enable-postgis.service"];
      path = [
         (pkgs.callPackage ./solver-env.nix {})
      ];
      script =
      let
      sem = ./SpatialEnergyModel;
      run-solver = pkgs.writeScript "run-solver"
          ''
          #! ${pkgs.bash}/bin/bash
          python ${sem}/pyomo/sem.py "$@"
          '';
      in
      ''
        export SOLVER_COUNT=16
        export DISABLE_CACHE=false
        export PG_HOST=127.0.0.1
        export SOLVER_COMMAND=${run-solver}
        ${pkgs.jre8_headless}/bin/java -jar ${../target/thermos.jar}
      '';
    };

    security.sudo.wheelNeedsPassword = false;
    
    users.motd = ''
    THERMOS demo server
    -------------------
    To restart the application: sudo systemctl restart thermos
    To restart the tileserver: sudo systemctl restart tiles
    
    Solver processes to look for: glpsol / python
    IF needs be, sudo pkill glpsol / pkill python will probably work

    To look in the db: psql -U postgres thermos_geometries

    Running / complete jobs will have data in /solver-work.
    If this directory doesn't exists, system won't start.
    
    /max_pixel_values.json must exist when tiles service starts.
    If it doesn't, it will be generated but the tileserver
    won't produce any tiles until either every tile has been
    considered, or you turn it off and on again.
    '';

    users.users.josh = {
       isNormalUser = true;
       extraGroups = ["wheel"];
       openssh.authorizedKeys.keys =
       [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDRkGcgihmDtnBlBlfuBMr61ShKhuGAPSptHpna5nTk/XgGtjAwBMo4trnCaXo2w3SOYwV6oIOV1gHy8Bsg/96zbpXPUHMAjMQ32p18bZCDsgCQhz5LroWAl2zz2MfEuQc35Wiy6zjFhAPQ6WbRfpiWGW97KZ0y0XG7XLwRj4J731CQN8tJBaPjQkKG4PPc1/mHfNjJ4l+v88lo6WZGHpInhOjVZjtYeYyVTHCFVZNPNDg5RM7nXvuMCqdzKnb6a5YKS1CcPKkQKG1zElSzA+kzC2QowOAjVwghm5+3Y3+826HElAdoGa7c0IGmv6PC4aTNHyjnHvUB0CZ+YIHn4YnGxnDWQBJRRKbjM5M99MtHY3cvYPR3TucJ+6XQ4ZLHsBSu0XZ8Uj4op1E3A2mie7Xw8k34A/mYig3E4AHa06M8libnsTkHq+Z0gCDM2zfq+3QIVse/e7M/PTdMhMq2FvbCiJ7HP4HyDYXeMNR35rPOi1GLopetqwFa/7ShPIWpq6y+Jh7PnCel+fSBxbaCsjuAqfJOrF88lwtZHszf0Sn4g3iXbdsdFRCqIitUpHcqL7RPJNJy0hFkhCT38lvvZ/dedrWX4gSBMSwyMt+yKvIaOsTVak5CihahKqfEDZZ7D4N1rrplITMFFtf5c93/dkWWU4RKZvzf9T8PJGovhW+ppQ==" ]
;
    };
  };
}