-- Implement segments_union aggregate, and its state function of the same name.

-- | Add a new segment to a list, merging overlapping or adjacent segments into
-- one.
--
-- We assume acc (the input list) is sorted and non-overlapping.
--
-- NB: This function is (only) designed to be used as the state-transition
-- function of the segments_union aggregate. In that usage, we can guarantee acc
-- is sorted and non-overlapping, because we are building it ourselves.
drop function if exists segments_union(segment[], segment) cascade;
create or replace function segments_union(acc segment[], seg segment)
returns segment[]
immutable strict language plpgsql
as $$
declare
    result segment[] := '{}';
    acc_length constant int := coalesce(array_length(acc, 1), 0);
    i int;
    tmp segment;
begin
    if coalesce(array_ndims(acc), 0) > 1 then
        raise exception 'Expected one-dimensional state';
    end if;
    if isempty(seg) then
        result := acc;
    else -- seg is non-empty range
        if acc_length = 0 then
            result := ARRAY[seg];
        else -- state is non-empty array
            -- Range operators used here are documented at
            -- https://www.postgresql.org/docs/9.6/static/functions-range.html#RANGE-OPERATORS-TABLE
            --
            -- (<<) : strictly left of
            -- (-|-): adjacent (a commutative operator)
            -- (&&) : overlaps (also commutative, obviously)

            -- 1. Skip over strictly less-than, unadjacent segments.
            i := 1;
            while acc[i] << seg and acc[i] -|- seg is false and i <= acc_length
            loop
                result := result || acc[i];
                i := i + 1;
            end loop;
            -- 2. Merge with overlapping/adjacent segments. (Or none, if seg
            --    falls directly between two or at an edge.)
            tmp := seg;
            while (tmp && acc[i] or tmp -|- acc[i]) and i <= acc_length
            loop
                tmp := acc[i] + tmp;
                i := i + 1;
            end loop;
            -- 3. Tack on the rest.
            result := result || tmp || acc[i : array_upper(acc, 1)];
        end if;
    end if;
    return result;
end
$$;

-- Now use the above to create the aggregate.
create aggregate "segments_union" (segment) (sfunc = segments_union, stype = segment[], initcond = '{}');

-- and now, a test
-- FIXME add this to the test suite
/*
select s.seg_union = '{"(-1,0)","(0,1)","(1,3)","(4,5)","(5,8]"}' as "test passed"
from (
    select segments_union(segval) seg_union
    from (values
        ('empty'::segment),
        ('(4, 5)'),
        ('(0, 1)'),
        ('(1, 2)'),
        ('(2, 3)'),
        ('(-1, 0)'),
        ('[2, 2]'),
        ('[6, 7]'),
        ('(5, 7]'),
        ('[6, 8]'),
        ('empty')
    ) as vals (segval)
) as s;
*/
