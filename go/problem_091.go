// Problem 91 - Right triangles with integer coordinates
//
// The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and
// are joined to the origin, O(0,0), to form ΔOPQ.
//
//             (https://projecteuler.net/project/images/p091_1.gif)
//
// There are exactly fourteen triangles containing a right angle that can be
// formed when each co-ordinate lies between 0 and 2 inclusive; that is,
// 0 ≤ x1, y1, x2, y2 ≤ 2.
//
//             (https://projecteuler.net/project/images/p091_2.gif)
//
// Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
package main

import (
	"fmt"
	"math"
	"sort"
)

type point struct {
	x, y int
}

func (p *point) IsEqual(other *point) bool {
	return p.x == other.x && p.y == other.y
}

func (p *point) DistanceSquared(other *point) int {
	return (other.x-p.x)*(other.x-p.x) + (other.y-p.y)*(other.y-p.y)
}

func (p *point) Slope(other *point) float64 {
	if other.x == p.x {
		return math.Inf(other.y - p.y)
	}
	return float64(other.y-p.y) / float64(other.x-p.x)
}

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	minX, minY, maxX, maxY := 0, 0, 50, 50
	o := &point{0, 0}

	for x1 := minX; x1 <= maxX; x1++ {
		for y1 := minY; y1 <= maxY; y1++ {
			p := &point{x1, y1}

			if p.IsEqual(o) {
				continue
			}

			for x2 := minX; x2 <= x1; x2++ {
				for y2 := minY; y2 <= maxY; y2++ {
					q := &point{x2, y2}

					if (p.x == q.x && p.y <= q.y) || q.IsEqual(o) {
						continue
					}

					if isTriangle(o, p, q) && isRight(o, p, q) {
						count++
					}
				}
			}
		}
	}

	return
}

func isTriangle(a, b, c *point) bool {
	if b.x != a.x {
		return a.Slope(b) != a.Slope(c)
	}
	return b.x-a.x != c.x-a.x
}

func isRight(a, b, c *point) bool {
	pts := []*point{a, b, c}

	sidesSq := make([]int, 3)
	for ix := 0; ix < 3; ix++ {
		sidesSq[ix] = pts[(ix+1)%3].DistanceSquared(pts[ix])
	}
	sort.Ints(sidesSq)

	return sidesSq[0]+sidesSq[1] == sidesSq[2]
}
