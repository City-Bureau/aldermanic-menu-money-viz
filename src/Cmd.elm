port module Cmd exposing (Ward, mapLoaded, selectWard, selectedWard)


type alias Ward =
    String


port selectWard : Ward -> Cmd a


port selectedWard : (Ward -> msg) -> Sub msg


port mapLoaded : (Bool -> msg) -> Sub msg
