// in which we have ways to assert that a given parser-produced structure meets
// some specific validity constraint expressed in the standard, and, for that
// constraint, what are the values represented?

// i don't know what's right here, so let's get it wrong a few times.

// try 1: a struct per contract, that has an impl that tries to pull out the
// details.

/// § 7.5.8 Cross-reference Streams, are all of an indirect stream-object,
/// a file-trailer dictionary, and contain cross-reference data that can
/// describe free objects, objects in object-streams, and plain old objects.
pub struct XRef {

}