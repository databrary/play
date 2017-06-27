// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package public

import (
	"bytes"
	"github.com/databrary/sqlboiler/boil"
	"github.com/databrary/sqlboiler/randomize"
	"github.com/databrary/sqlboiler/strmangle"
	"github.com/pmezard/go-difflib/difflib"
	"os"
	"os/exec"
	"reflect"
	"sort"
	"strings"
	"testing"
)

func testVolumeCitations(t *testing.T) {
	t.Parallel()

	query := VolumeCitations(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testVolumeCitationsLive(t *testing.T) {
	all, err := VolumeCitations(dbMain.liveDbConn).All()
	if err != nil {
		t.Fatalf("failed to get all VolumeCitations err: ", err)
	}
	tx, err := dbMain.liveTestDbConn.Begin()
	if err != nil {
		t.Fatalf("failed to begin transaction: ", err)
	}
	for _, v := range all {
		err := v.Insert(tx)
		if err != nil {
			t.Fatalf("failed to failed to insert %s because of %s", v, err)
		}

	}
	err = tx.Commit()
	if err != nil {
		t.Fatalf("failed to commit transaction: ", err)
	}
	bf := &bytes.Buffer{}
	dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM volume_citation) TO STDOUT" -d `, dbMain.DbName)
	dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
	dumpCmd.Stdout = bf
	err = dumpCmd.Start()
	if err != nil {
		t.Fatalf("failed to start dump from live db because of %s", err)
	}
	dumpCmd.Wait()
	if err != nil {
		t.Fatalf("failed to wait dump from live db because of %s", err)
	}
	bg := &bytes.Buffer{}
	dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM volume_citation) TO STDOUT" -d `, dbMain.LiveTestDBName)
	dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
	dumpCmd.Stdout = bg
	err = dumpCmd.Start()
	if err != nil {
		t.Fatalf("failed to start dump from test db because of %s", err)
	}
	dumpCmd.Wait()
	if err != nil {
		t.Fatalf("failed to wait dump from test db because of %s", err)
	}
	bfslice := sort.StringSlice(difflib.SplitLines(bf.String()))
	gfslice := sort.StringSlice(difflib.SplitLines(bg.String()))
	bfslice.Sort()
	gfslice.Sort()
	diff := difflib.ContextDiff{
		A:        bfslice,
		B:        gfslice,
		FromFile: "databrary",
		ToFile:   "test",
		Context:  1,
	}
	result, _ := difflib.GetContextDiffString(diff)
	if len(result) > 0 {
		t.Fatalf("VolumeCitationsLive failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
	}

}

func testVolumeCitationsDelete(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeCitation.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeCitationsQueryDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeCitations(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeCitationsSliceDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeCitationSlice{volumeCitation}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testVolumeCitationsExists(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := VolumeCitationExists(tx, volumeCitation.Volume)
	if err != nil {
		t.Errorf("Unable to check if VolumeCitation exists: %s", err)
	}
	if !e {
		t.Errorf("Expected VolumeCitationExistsG to return true, but got false.")
	}
}

func testVolumeCitationsFind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	volumeCitationFound, err := FindVolumeCitation(tx, volumeCitation.Volume)
	if err != nil {
		t.Error(err)
	}

	if volumeCitationFound == nil {
		t.Error("want a record, got nil")
	}
}

func testVolumeCitationsBind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = VolumeCitations(tx).Bind(volumeCitation); err != nil {
		t.Error(err)
	}
}

func testVolumeCitationsOne(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := VolumeCitations(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testVolumeCitationsAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitationOne := &VolumeCitation{}
	volumeCitationTwo := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitationOne, volumeCitationDBTypes, false, volumeCitationColumnsWithDefault...); err != nil {

		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeCitationTwo, volumeCitationDBTypes, false, volumeCitationColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitationOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeCitationTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeCitations(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testVolumeCitationsCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitationOne := &VolumeCitation{}
	volumeCitationTwo := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitationOne, volumeCitationDBTypes, false, volumeCitationColumnsWithDefault...); err != nil {

		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}
	if err = randomize.Struct(seed, volumeCitationTwo, volumeCitationDBTypes, false, volumeCitationColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitationOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = volumeCitationTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func volumeCitationBeforeInsertHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationAfterInsertHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationAfterSelectHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationBeforeUpdateHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationAfterUpdateHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationBeforeDeleteHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationAfterDeleteHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationBeforeUpsertHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func volumeCitationAfterUpsertHook(e boil.Executor, o *VolumeCitation) error {
	*o = VolumeCitation{}
	return nil
}

func testVolumeCitationsHooks(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	empty := &VolumeCitation{}

	AddVolumeCitationHook(boil.BeforeInsertHook, volumeCitationBeforeInsertHook)
	if err = volumeCitation.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationBeforeInsertHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.AfterInsertHook, volumeCitationAfterInsertHook)
	if err = volumeCitation.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationAfterInsertHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.AfterSelectHook, volumeCitationAfterSelectHook)
	if err = volumeCitation.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationAfterSelectHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.BeforeUpdateHook, volumeCitationBeforeUpdateHook)
	if err = volumeCitation.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationBeforeUpdateHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.AfterUpdateHook, volumeCitationAfterUpdateHook)
	if err = volumeCitation.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationAfterUpdateHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.BeforeDeleteHook, volumeCitationBeforeDeleteHook)
	if err = volumeCitation.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationBeforeDeleteHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.AfterDeleteHook, volumeCitationAfterDeleteHook)
	if err = volumeCitation.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationAfterDeleteHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.BeforeUpsertHook, volumeCitationBeforeUpsertHook)
	if err = volumeCitation.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationBeforeUpsertHooks = []VolumeCitationHook{}

	AddVolumeCitationHook(boil.AfterUpsertHook, volumeCitationAfterUpsertHook)
	if err = volumeCitation.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(volumeCitation, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", volumeCitation)
	}
	volumeCitationAfterUpsertHooks = []VolumeCitationHook{}
}
func testVolumeCitationsInsert(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeCitationsInsertWhitelist(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx, volumeCitationColumns...); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testVolumeCitationToOneVolumeUsingVolume(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign Volume
	var local VolumeCitation

	foreignBlacklist := volumeColumnsWithDefault
	if err := randomize.Struct(seed, &foreign, volumeDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}
	localBlacklist := volumeCitationColumnsWithDefault
	if err := randomize.Struct(seed, &local, volumeCitationDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Volume = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.VolumeByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := VolumeCitationSlice{&local}
	if err = local.L.LoadVolume(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Volume == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Volume = nil
	if err = local.L.LoadVolume(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Volume == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testVolumeCitationToOneSetOpVolumeUsingVolume(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a VolumeCitation
	var b, c Volume

	foreignBlacklist := strmangle.SetComplement(volumePrimaryKeyColumns, volumeColumnsWithoutDefault)
	if err := randomize.Struct(seed, &b, volumeDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, volumeDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Volume struct: %s", err)
	}
	localBlacklist := strmangle.SetComplement(volumeCitationPrimaryKeyColumns, volumeCitationColumnsWithoutDefault)
	if err := randomize.Struct(seed, &a, volumeCitationDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Volume{&b, &c} {
		err = a.SetVolume(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Volume != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.VolumeCitation != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Volume != x.ID {
			t.Error("foreign key was wrong value", a.Volume)
		}

		if exists, err := VolumeCitationExists(tx, a.Volume); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}

func testVolumeCitationsReload(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = volumeCitation.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeCitationsReloadAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := VolumeCitationSlice{volumeCitation}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeCitationsSelect(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := VolumeCitations(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	volumeCitationDBTypes = map[string]string{`Head`: `text`, `URL`: `text`, `Volume`: `integer`, `Year`: `smallint`}
	_                     = bytes.MinRead
)

func testVolumeCitationsUpdate(t *testing.T) {
	t.Parallel()

	if len(volumeCitationColumns) == len(volumeCitationPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := volumeCitationColumnsWithDefault

	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	if err = volumeCitation.Update(tx); err != nil {
		t.Error(err)
	}
}

func testVolumeCitationsSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(volumeCitationColumns) == len(volumeCitationPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := volumeCitationPrimaryKeyColumns

	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(volumeCitationColumns, volumeCitationPrimaryKeyColumns) {
		fields = volumeCitationColumns
	} else {
		fields = strmangle.SetComplement(
			volumeCitationColumns,
			volumeCitationPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(volumeCitation))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := VolumeCitationSlice{volumeCitation}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}

func testVolumeCitationsUpsert(t *testing.T) {
	t.Parallel()

	if len(volumeCitationColumns) == len(volumeCitationPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	volumeCitation := &VolumeCitation{}
	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, true); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = volumeCitation.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeCitation: %s", err)
	}

	count, err := VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	blacklist := volumeCitationPrimaryKeyColumns

	if err = randomize.Struct(seed, volumeCitation, volumeCitationDBTypes, false, blacklist...); err != nil {
		t.Errorf("Unable to randomize VolumeCitation struct: %s", err)
	}

	if err = volumeCitation.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert VolumeCitation: %s", err)
	}

	count, err = VolumeCitations(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
