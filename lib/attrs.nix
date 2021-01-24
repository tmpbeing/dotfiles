# All credits for this file goes to hlissner

with builtins;
with lib;
rec {
  # mapFilterAttrs ::
  #   (name -> value -> bool)
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs = pred: f: attrs: filtersAttrs pred (mapAttrs' f attrs);
}
