import shared.*

type Probe = Point
type Velocity = Point
case class Target(xmin:Int, xmax:Int, ymin:Int, ymax:Int)

def data(f:String) = 
    val parser = "target area: x=(\\d+)..(\\d+), y=-(\\d+)..-(\\d+)".r
    val line = Input.asNonEmptyLines(f).head 
    line match { case parser(xmin, xmax, ymin, ymax) => 
        Target(xmin.toInt, xmax.toInt, -ymin.toInt, -ymax.toInt) }

def isPastTarget(p:Probe, t:Target) = p.x > t.xmax || p.y < t.ymin
def isOnTarget(p:Probe, t:Target)   = p.x >= t.xmin && p.x <= t.xmax && 
                                      p.y >= t.ymin && p.y <= t.ymax
def isEverOnTarget(p:Probe, v:Velocity, t:Target):Boolean = 
    if(isPastTarget(p,t)) false 
    else isOnTarget(p,t) || 
         isEverOnTarget(p + v, Point(v.x - 1 max 0, v.y - 1), t) 
def points(xmin:Int, xmax:Int, ymin:Int, ymax:Int) =  
    for (x <- xmin to xmax; y <- ymin to ymax) 
    yield Point(x,y)
def onTargetVelocities(t:Target) = 
    val velocities = Point.range(1, t.xmax,t.ymin, 2 * t.xmax)
    velocities.filter(v => isEverOnTarget(Point(0,0),v,t))

def part1(f:String) = onTargetVelocities(data(f)).map(_._2).max.triangleNumber
def part2(f:String) = onTargetVelocities(data(f)).length

Output.printResults(17,part1,part2)

//===============================================================
// TESTS : Trying it out with worksheets
//===============================================================
val testTarget = Target(20,30,-10,-5)

//given by description
assert(isEverOnTarget(Point(0,0),Point(7,2),testTarget))
assert(isEverOnTarget(Point(0,0),Point(6,3),testTarget))
assert(isEverOnTarget(Point(0,0),Point(9,0),testTarget))
assert(isEverOnTarget(Point(0,0),Point(17,-4),testTarget) == false)

//unit tests
val xmid = 25; val xedge = 20; val xpast = 35
val ymid = -5; val yedge = -10; val ypast = -15
assert(isEverOnTarget(Point(xpast,ymid),Point(0,0),testTarget) == false)
assert(isEverOnTarget(Point(xmid,ypast),Point(0,0),testTarget) == false)
assert(isOnTarget(Point(xmid,ymid),testTarget))
assert(isOnTarget(Point(xedge,yedge),testTarget))
assert(isOnTarget(Point(xedge,ypast),testTarget)==false)
assert(isOnTarget(Point(xpast,yedge),testTarget)==false)
