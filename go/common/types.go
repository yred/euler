package common

type bitSet struct {
	size int
	data []uint64
}

func NewBitSet(size int) *bitSet {
	return &bitSet{size, make([]uint64, (size/8)+1)}
}

func (b *bitSet) Get(index int) bool {
	return b.data[index/64]&(uint64(1)<<uint(index%64)) > 0
}

func (b *bitSet) Set(index int, value bool) {
	bval := uint64(1) << uint(index%64)

	if value {
		b.data[index/64] |= bval
	} else {
		bval = ^bval
		b.data[index/64] &= bval
	}
}

func (b *bitSet) SetAll(value bool) {
	bval := uint64(0)

	if value {
		bval = ^bval
	}

	for ix := range b.data {
		b.data[ix] = bval
	}
}

func (b *bitSet) Iterate(fn func(int, bool)) {
	for ix, val := range b.data {
		stopIndex := (ix + 1) * 64
		if stopIndex > b.size {
			stopIndex = b.size
		}

		for iy, iz := 0, ix*64; iz < stopIndex; iy, iz = iy+1, iz+1 {
			fn(iz, val&(uint64(1)<<uint(iy)) > 0)
		}
	}
}
