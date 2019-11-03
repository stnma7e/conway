//..           .
//..    .     .      .
//..  .. . ..      .
//.       .. ..   . ..
//.   .          .   .
//.   .
//.    ...     ....
//.                .

// An implementation of Conway's Game of Life.
// Taken from golang.org and edited.
package main

import (
	"bytes"
	"fmt"
	"math"
	"math/rand"
	"time"
)

// Field represents a two-dimensional field of cells.
type Field struct {
	s    [][]bool
	w, h int
}

// NewField returns an empty field of the specified width and height.
func NewField(w, h int) *Field {
	s := make([][]bool, h)
	for i := range s {
		s[i] = make([]bool, w)
	}
	return &Field{s: s, w: w, h: h}
}

// Set sets the state of the specified cell to the given value.
func (f *Field) Set(x, y int, b bool) {
	f.s[y][x] = b
}

// Alive reports whether the specified cell is alive.
// If the x or y coordinates are outside the field boundaries they are wrapped
// toroidally. For instance, an x value of -1 is treated as width-1.
func (f *Field) Alive(x, y int) bool {
	x += f.w
	x %= f.w
	y += f.h
	y %= f.h
	return f.s[y][x]
}

type InfluenceFunc func(x, y int) float64

// Next returns the state of the specified cell at the next time step.
func (f *Field) Next(x, y, searchRadius int, influence InfluenceFunc) bool {
	// Count the adjacent cells that are alive.
	neighbors := 0.0
	for i := -searchRadius; i <= searchRadius; i++ {
		for j := -searchRadius; j <= searchRadius; j++ {
			if (j != 0 || i != 0) && f.Alive(x+i, y+j) {
				influenced := influence(i, j)
				//fmt.Print(influenced, " ")
				neighbors += influenced
			}
		}
	}
	// Return next state according to the game rules:
	//   exactly 3 neighbors: on,
	//   exactly 2 neighbors: maintain current state,
	//   otherwise: off.

	//return neighbors == 3 || neighbors == 2 && f.Alive(x, y)

	// truncate neighbors
	neighbors = float64(int(neighbors))
	alive := 0.0

	switch neighbors {
	case 8:
		alive = 0.000
	case 7:
		alive = 0.001
	case 6:
		alive = 0.01
	case 5:
		alive = 0.02
	case 4:
		alive = 0.04
	case 3:
		alive = 0.95
	case 2:
		alive = 0.9
	case 1:
		alive = 0.1
	case 0:
		alive = 0.0000
	}

	//fmt.Print(alive, " ")
	probabilityAlive := rand.Float64()

	if alive > probabilityAlive {
		if neighbors < 3 {
			return f.Alive(x, y)
		} else {
			return true
		}
	}

	return false
}

// Life stores the state of a round of Conway's Game of Life.
type Life struct {
	a, b *Field
	w, h int
}

// NewLife returns a new Life game state with a random initial state.
func NewLife(w, h int) *Life {
	a := NewField(w, h)
	for i := 0; i < (w * h / 4); i++ {
		a.Set(rand.Intn(w), rand.Intn(h), true)
	}
	return &Life{
		a: a, b: NewField(w, h),
		w: w, h: h,
	}
}

var influenceMap map[int]float64 = map[int]float64{
	0: 1.0,
	1: 1.0,
	2: 0.5,
	3: 0.25,
	4: 0.125,
}

// Step advances the game by one instant, recomputing and updating all cells.
func (l *Life) Step() {
	// Update the state of the next field (b) from the current field (a).
	for y := 0; y < l.h; y++ {
		for x := 0; x < l.w; x++ {
			l.b.Set(x, y, l.a.Next(x, y, 2, func(x, y int) float64 {
				absX, absY := int(math.Abs(float64(x))), int(math.Abs(float64(y)))
				valX, valY := influenceMap[absX], influenceMap[absY]
				avg := valX + valY
				return avg / 2
			}))
		}
	}
	// Swap fields a and b.
	l.a, l.b = l.b, l.a
}

// String returns the game board as a string.
func (l *Life) String() string {
	var buf bytes.Buffer
	for y := 0; y < l.h; y++ {
		for x := 0; x < l.w; x++ {
			b := byte(' ')
			if l.a.Alive(x, y) {
				b = '*'
			}
			buf.WriteByte(b)
		}
		buf.WriteByte('\n')
	}
	return buf.String()
}

type LifeResult struct {
	life, lifeBefore          *Life
	iterationsUntilExtinction int
}

func main() {
	//rand.Seed(time.Now().UnixNano())
	lives := make([]LifeResult, 100)

	num := 0
	for i := range lives {
		lives[i].life = NewLife(40, 15)
		lives[i] = runLife(lives[i].life, 1000)
		num += lives[i].iterationsUntilExtinction
		fmt.Println(lives[i].lifeBefore, "\n________________________________________\n")
	}

	num /= len(lives)
	fmt.Println(num)
}

func runLife(l *Life, stopAfterIteration int) LifeResult {
	iterations := 0
	death := true
	lifeBefore := &Life{}
	for i := 0; i < stopAfterIteration; i++ {
		l.Step()

		for j := range l.a.s {
			for k := range l.a.s[j] {
				if l.a.s[j][k] == true {
					death = false
					break
				}
			}
		}

		if death {
			iterations = i
			break
		}
		death = true

		fmt.Print("\n___________________________________________\n", l) // Clear screen and print field.
		time.Sleep(time.Second / 10)

		lifeBefore = l
	}

	return LifeResult{l, lifeBefore, iterations}
}
