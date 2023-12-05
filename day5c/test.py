text = get_input(5, 2023)
chunks = schunks(text)

seeds, chunks = chunks[0], chunks[1:]
seeds = seeds.split(':')[1]
seeds = numlist(seeds)

def apply(mapping, v):
    for dstart, sstart, length in mapping:
        rel = v - sstart
        if 0 <= rel < length:
            return dstart + rel
    return v

def apply_to_range(mapping, input_ranges):

    while input_ranges:

        input_range = input_ranges.pop()

        for dstart, sstart, length in mapping:
            if input_range.empty:
                break

            source_range = Range(sstart, sstart + length - 1)

            hit = input_range.intersect(source_range)
            if not hit.empty:

                yield Range(hit.min - sstart + dstart, hit.max - sstart + dstart)

                if input_range.min < hit.min:
                    input_ranges.append(Range(input_range.min, hit.min - 1))
                if input_range.max > hit.max:
                    input_ranges.append(Range(hit.max + 1, input_range.max))

                input_range.min = input_range.max = None

        if not input_range.empty:
            yield input_range


mappings = []
for chunk in chunks:

    mapping = []

    chunk = chunk.split('\n')[1:]
    for line in chunk:
        line = numlist(line)
        mapping.append(line)
        #dstart, sstart, length = line

    mappings.append(mapping)

final_ranges = []
for start, length in paginate(seeds, 2):

    in_ranges = [Range(start, start + length - 1)]

    for mapping in mappings:

        out_ranges = []

        for out_range in apply_to_range(mapping, in_ranges):
            out_ranges.append(out_range)

        in_ranges = out_ranges

    final_ranges += out_ranges

ans(min(r.min for r in final_ranges))
