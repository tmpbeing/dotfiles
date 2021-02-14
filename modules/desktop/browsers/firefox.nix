{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.firefox;
in {
  options.modules.desktop.browsers.firefox = with types; {
    enable = mkBoolOpt false;
    profileName = mkOpt types.str config.user.name;

    settings = mkOpt' (attrsOf (oneOf [ bool int str ])) { } ''
      Firefox preferences to set in <filename>user.js</filename>
    '';
    extraConfig = mkOpt' lines "" ''
      Extra lines to add to <filename>user.js</filename>
    '';

    userChrome = mkOpt' lines "" "CSS Styles for Firefox's interface";
    userContent = mkOpt' lines "" "Global CSS Styles for websites";
  };

  config = mkIf cfg.enable (mkMerge [{
    user.packages = with pkgs; [
      firefox-bin
      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = " Open a private Firefox window";
        icon = "firefox";
        exec = "${firefox-bin}/bin/firefox --private-window";
        categories = "Network";
      })
    ];

    # Avoid creating ~/Desktop
    env.XDG_DESKTOP_DIR = "$HOME/";

    modules.desktop.browsers.firefox.settings = {
      "devtools.theme" = "dark";
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      "browser.download.dir" =
        "${config.user.home}/download"; # Stop creating ~/Downloads
      "signon.remembersSignons" = false;
      "browser.newtabpage.enabled" = false;
      "browser.newtab.url" = "about:blank";
      "browser.newtabpage.activity-stream.enabled" = false;
      "browser.newtabpage.enhanced" = false;
      "browser.newtab.preload" = false;
      "browser.newtagpage.directory.ping" = "";
      "browser.newtagpage.directory.source" = "data:text/plain,{}";
      "extensions.htmlaboutaddons.recommendations.enabled" = false;
      "extensions.htmlaboutaddons.discover.enabled" = false;
      "extensions.pocket.enabled" = false;
      "app.normandy.enabled" = false;
      "app.normandy.api_url" = "";
      "extensions.shield-recipe-client.enabled" = false;
      "app.shield.optoutstudies.enabled" = false;
      "dom.battery.enabled" = false;
      "beacon.enabled" = false;
      "browser.send_pings" = false;
      "dom.gamepad.enabled" = false;
      "browser.fixup.alternate.enabled" = false;
      "toolkit.telemetry.enabled" = false;
      "toolkit.telemetry.unified" = false;
      "toolkit.telemetry.archive_enabled" = false;
      "experiments.supported" = false;
      "experiments.enabled" = false;
      "experiments.manifest.uri" = "";
      "datareporting.healthreport.uploadEnabled" = false;
      "datareporting.healthreport.service.enabled" = false;
      "datareporting.policy.dataSubmissionEnabled" = false;
    };

    home.file = let cfgPath = ".mozilla/firefox";
    in {
      "${cfgPath}/profiles.ini".text = ''
        [Profile0]
        Name=default
        IsRelative=1
        Path=${cfg.profileName}.default
        Default=1

        [General]
        StartWithLastProfile=1
        Version=2
      '';

      "${cfgPath}/${cfg.profileName}.default/user.js" =
        mkIf (cfg.settings != { } || cfg.extraConfig != "") {
          text = ''
            ${concatStrings (mapAttrsToList (name: value: ''
              user_pref("${name}", ${builtins.toJSON value});
            '') cfg.settings)}
            ${cfg.extraConfig}
          '';
        };

      "${cfgPath}/${cfg.profileName}.default/chrome/userChrome.css" =
        mkIf (cfg.userChrome != "") { text = cfg.userChrome; };

      "${cfgPath}/${cfg.profileName}.default/chrome/userContent.css" =
        mkIf (cfg.userContent != "") { text = cfg.userContent; };
    };
  }]);
}
