open AbsDom

type dir = string

type fvector = bool list
type tdata = (fvector * bool)

type locset = Loc.t BatSet.t
