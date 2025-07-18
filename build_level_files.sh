#!/usr/bin/env bash

set -euxo pipefail

LEVEL=$(cat <<EOF
import Submarination.Biome.Gen
import Submarination.Biome.KelpForest ( kelpForestGen )
import Submarination.Biome.AncientCaves ( ancientCavesGen )
import Submarination.Biome.IntertidalZone ( intertidalZoneGen )

toHaskellSourceFile "src/Submarination/Biome/KelpForestGen.hs" "Submarination.Biome.KelpForestGen" kelpForestGen

toHaskellSourceFile "src/Submarination/Biome/AncientCavesGen.hs" "Submarination.Biome.AncientCavesGen" ancientCavesGen

toHaskellSourceFile "src/Submarination/Biome/IntertidalZoneGen.hs" "Submarination.Biome.IntertidalZoneGen" intertidalZoneGen
EOF
)

echo "$LEVEL" | cabal repl lib:submarination

