let latest = 
    import (builtins.fetchGit {
        url = https://github.com/NixOS/nixpkgs-channels;
        ref = "nixos-19.03";
        rev = "34c7eb7545d155cc5b6f499b23a7cb1c96ab4d59";
    }) {} ;

old = import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-14.12";
    rev = "b373bf9c7d54827504781009efbb4bee15856d86";
}) {};

in

with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-14.12";
    rev = "b373bf9c7d54827504781009efbb4bee15856d86";
}) {};

latest.haskell.lib.buildStackProject {
    name = "crm";
#     ghc = old.haskell.compiler.ghc783;
    buildInputs = [ ghc gd fontconfig freetype expat postgresql icu ncurses haskellPackages.happy ];
}
