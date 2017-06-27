// Package xo_models contains the types for schema 'public'.
package xo_models

import (
	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/sqlboiler/boil"
)

// GENERATED BY XO. DO NOT EDIT.

// SegmentsUnion calls the stored procedure 'public.segments_union(segment, segment[], segment, segment[], segment[]) segment[]' on db.
func SegmentsUnion(exec boil.Executor, v0 custom_types.Segment, v1 []custom_types.Segment, v2 custom_types.Segment, v3 []custom_types.Segment, v4 []custom_types.Segment) ([]custom_types.Segment, error) {
	var err error

	// sql query
	const query = `SELECT public.segments_union($1, $2, $3, $4, $5)`

	// run query
	var ret []custom_types.Segment

	err = exec.QueryRow(query, v0, v1, v2, v3, v4).Scan(&ret)
	if err != nil {
		return nil, err
	}

	return ret, nil
}
