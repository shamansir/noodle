export function everyP (now) {
    return function(constant) {
        return function (t) {
            var out = constant(now())
            var intervalId = setInterval(function () {
                out.set(now())
            }, t)
            var cancelF = function() { clearInterval(intervalId); }
            return { signal : out, cancel : cancelF }
        }
    }
}