-- Implement segments_union(segment[], segment) in plpgsql. Remove the unused
-- segments_union(segment[], segment[]).

-- Fare thee well
drop function if exists segments_union(segment[], segment[]);

-- | Add a new segment to a list, merging overlapping or adjacent segments into
-- one.
--
-- We assume acc (the input list) is sorted and non-overlapping.
create or replace function segments_union(acc segment[], seg segment)
returns segment[]
immutable strict parallel safe language plpgsql
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
            -- 1. Skip over strictly less-than, unadjacent segments
            i := 1;
            while acc[i] << seg and acc[i] -|- seg is false and i <= acc_length
            loop
                result := result || acc[i];
                i := i + 1;
            end loop;
            -- 2. Merge with overlapping/adjacent segments. (Or none, if seg
            --    fell directly between two.)
            tmp := seg;
            while (tmp && acc[i] or tmp -|- acc[i]) and i <= acc_length
            loop
                tmp := acc[i] + tmp;
                i := i + 1;
            end loop;
            -- 3. Tack on the rest.
            result := result || tmp || acc[i :];
        end if;
    end if;
    return result;
end
$$;

-- and now, a test
-- FIXME add this to the test suite
/*
create temporary table segs ( s segment );
insert into segs values ('empty'), ('(4,5)'),('(0,1)'), ('(1,2)'), ('(2,3)'), ('(-1,0)'), ('[2,2]'), ('[6,7]'),('(5,7]'),('[6,8]'), ('empty');
select ss.s = '{"(-1,0)","(0,1)","(1,3)","(4,5)","(5,8]"}' as "test passed"
from (select segments_union(s) s from segs) as ss;
*/
