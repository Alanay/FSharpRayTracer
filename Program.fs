open System.IO
open System.Drawing

[<Struct>]
type Vector = 
    { X: double; Y: double; Z: double }
    static member (/) (v, d) = { X = v.X / d; Y = v.Y / d; Z = v.Z / d }
    static member (*) (v, d) = { X = v.X * d; Y = v.Y * d; Z = v.Z * d }
    static member (+) (v1, v2) = { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
    static member (-) (v1, v2) = { X = v1.X - v2.X; Y = v1.Y - v2.Y; Z = v1.Z - v2.Z }
    static member (^^) (v1, v2) = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z
    member this.Magnitude = sqrt(this.X * this.X + this.Y * this.Y + this.Z * this.Z)
    member this.Normalize =
        let mg = this.Magnitude
        { X = this.X / mg; Y = this.Y / mg; Z = this.Z / mg }
    member this.Clamp =
        let minmax = max 0.0 << min 255.0
        { X = minmax this.X; Y = minmax this.Y; Z = minmax this.Z }

[<Struct>]
type Ray =
    { Origin: Vector; Direction: Vector }

[<Struct>]
type Sphere =
    { Center: Vector; Radius: double }
    member this.GetNormal p = (p - this.Center) / this.Radius
    member this.Intersect r =
        let o = r.Origin
        let d = r.Direction
        let oc = o - this.Center
        let b = (oc ^^ d) * 2.0
        let c = (oc ^^ oc) - pown this.Radius 2
        let disc = (pown b 2) - 4.0 * c
        if disc < 1e-4 then
            None
        else
            let disc = sqrt(disc)
            let t0 = - b - disc
            let t1 = - b + disc
            let t = if t0 < t1 then t0 else t1
            Some t

let v x y z = { X = x; Y = y; Z = z }

let w = 500
let h = 500

let bitmap = new Bitmap(w, h)

let sphere = { Center = v 250.0 250.0 50.0; Radius = 50.0}
let light = { Center = v 0.0 0.0 50.0; Radius = 1.0}

let black = v 0.0 0.0 0.0
let white = v 255.0 255.0 255.0
let red = v 255.0 0.0 0.0

let color v = Color.FromArgb(255, int v.X, int v.Y, int v.Z)

for x in 0 .. w - 1 do
    for y in 0 .. h - 1 do
        let ray = { Origin = v (double x) (double y) 0.0; Direction = v 0.0 0.0 1.0}
        match sphere.Intersect ray with
        | Some t ->
            let p = (ray.Direction * t) + ray.Origin
            let l = light.Center - p
            let n = sphere.GetNormal p
            let dt = l.Normalize ^^ n.Normalize
            let pixelColor = ((red + white * dt) * 0.5).Clamp
            bitmap.SetPixel(x, y, color pixelColor)
        | None ->
            bitmap.SetPixel(x, y, color black)

bitmap.Save(Path.Combine(__SOURCE_DIRECTORY__, "bitmap.png"))
