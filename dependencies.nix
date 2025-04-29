{pkgs}:
let
  locale = pkgs.lib.strings.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
   packages = with pkgs; [
	   picocom
	   stlink
	   stlink-gui
     scree
   ];
in
{
    default = pkgs.mkShell {
      LOCALE_ARCHIVE = locale;
      packages = packages;
    };
}
