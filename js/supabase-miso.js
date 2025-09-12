// dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {
  globalThis['supabase'][namespace][fnName].apply(this, args).then(({ data, error }) => {
    if (data) successful(data);
    if (error) errorful(error);
  });
}

globalThis['runSupabaseQuery'] = function (from, fnName, args, successful, errorful) {
  globalThis['supabase']['from'](from)[fnName].apply(this, args).then(({ data, error }) => {
    if (data) successful(data);
    if (error) errorful(error);
  });
}

globalThis['runSupabaseFrom'] = function (namespace, from, fnName, args, successful, errorful) {
  globalThis['supabase'][namespace]['from'](from)[fnName].apply(this, args).then(({ data, error }) => {
    if (data) successful(data);
    if (error) errorful(error);
  });
}
