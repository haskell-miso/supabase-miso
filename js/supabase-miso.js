// The glue between supabase-miso and supabase.js (globalThis.supabase).
//
// Every call reports back through two callbacks: successful(data) when the
// result has no error (data can be null: an insert without .select(), or
// signOut), and errorful(message) when it has one, when its promise is
// rejected (a network error), or when the call throws.  A result that is not
// a promise (getPublicUrl, onAuthStateChange) is reported the same way.

function errorMessage(e) {
  if (e && typeof e.message === "string") return e.message;
  return String(e);
}

function settle(run, successful, errorful) {
  let result;
  try {
    result = run();
  } catch (e) {
    errorful(errorMessage(e));
    return;
  }
  Promise.resolve(result).then(
    (r) => {
      if (r && r.error) errorful(errorMessage(r.error));
      else successful(r && r.data !== undefined ? r.data : null);
    },
    (e) => errorful(errorMessage(e))
  );
}

function applyFilters(query, filters) {
  (filters || []).forEach((filter) => {
    query = query[filter.operator](filter.column, filter.value);
  });
  return query;
}

// runSupabase('auth', 'signUp', args, successful, errorful)
globalThis["runSupabase"] = function (namespace, fnName, args, successful, errorful) {
  settle(() => globalThis["supabase"][namespace][fnName](...args), successful, errorful);
};

// runSupabaseFrom('storage', bucket, 'upload', args, successful, errorful)
globalThis["runSupabaseFrom"] = function (namespace, fromArg, fnName, args, successful, errorful) {
  settle(() => globalThis["supabase"][namespace].from(fromArg)[fnName](...args), successful, errorful);
};

// runSupabaseQuery(table, 'insert', args, successful, errorful)
globalThis["runSupabaseQuery"] = function (from, fnName, args, successful, errorful) {
  settle(() => globalThis["supabase"].from(from)[fnName](...args), successful, errorful);
};

// runSupabaseSelect(table, columns, [filters, { count, head }], successful, errorful)
globalThis["runSupabaseSelect"] = function (table, columns, args, successful, errorful) {
  settle(() => {
    const [filters, options] = args;
    return applyFilters(globalThis["supabase"].from(table).select(columns, options || {}), filters);
  }, successful, errorful);
};

// runSupabaseUpdate(table, [values, filters, { count }], successful, errorful)
globalThis["runSupabaseUpdate"] = function (table, args, successful, errorful) {
  settle(() => {
    const [values, filters, options] = args;
    return applyFilters(globalThis["supabase"].from(table).update(values, options || {}), filters);
  }, successful, errorful);
};

// runSupabaseDelete(table, [filters, { count }], successful, errorful)
globalThis["runSupabaseDelete"] = function (table, args, successful, errorful) {
  settle(() => {
    const [filters, options] = args;
    return applyFilters(globalThis["supabase"].from(table).delete(options || {}), filters);
  }, successful, errorful);
};
