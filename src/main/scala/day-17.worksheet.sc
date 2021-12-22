import shared.*

case class Probe(x:Int, y:Int){
    def inc(v:Velocity) = Probe(this.x + v.x, this.y + v.y)
    def past(t:Target) = this.x > t.xmax || this.y < t.ymin
    def on(t:Target):Boolean = 
        this.x >= t.xmin && this.x <= t.xmax && 
        this.y >= t.ymin && this.y <= t.ymax
}
case class Velocity(x:Int, y:Int){
    def inc = Velocity(this.x-1 max 0, this.y -1)
}
case class Target(xmin:Int, xmax:Int, ymin:Int, ymax:Int)

def data(f:String) = 
    val parser = "target area: x=(\\d+)..(\\d+), y=-(\\d+)..-(\\d+)".r
    val line = Input.asNonEmptyLines(f).head 
    line match { case parser(xmin, xmax, ymin, ymax) => 
        Target(xmin.toInt, xmax.toInt, -ymin.toInt, -ymax.toInt) }

def hits(p:Probe, v:Velocity, t:Target):Boolean = 
    if(p.past(t)) false else p.on(t) || hits(p.inc(v), v.inc, t) 

def findHits(t:Target) = 
    val vs = for (x <- 1 to t.xmax; y <- t.ymin to (20 * t.xmax)) yield Velocity(x,y)
    vs.filter(v => hits(Probe(0,0),v,t))

def findHighestHit(vs:Iterable[Velocity]) = 
    val y = vs.map(_._2).max
    (y * (y + 1)) / 2

def part1(f:String) = findHighestHit(findHits(data(f)))
def part2(f:String) = findHits(data(f)).length

Output.printResults(17,part1,part2)

//===============================================================
// TESTS : Trying it out with worksheets
//===============================================================
val testTarget = data("17e")
//given by description
assert(hits(Probe(0,0),Velocity(7,2),testTarget))
assert(hits(Probe(0,0),Velocity(6,3),testTarget))
assert(hits(Probe(0,0),Velocity(9,0),testTarget))
assert(hits(Probe(0,0),Velocity(17,-4),testTarget) == false)
//units
val xmid = 25; val xedge = 20; val xpast = 35
val ymid = -5; val yedge = -10; val ypast = -15
assert(hits(Probe(xpast,ymid),Velocity(0,0),testTarget) == false)
assert(hits(Probe(xmid,ypast),Velocity(0,0),testTarget) == false)
assert(Probe(xmid,ymid).on(testTarget))
assert(Probe(xedge,yedge).on(testTarget))
assert(Probe(xedge,ypast).on(testTarget)==false)
assert(Probe(xpast,yedge).on(testTarget)==false)
