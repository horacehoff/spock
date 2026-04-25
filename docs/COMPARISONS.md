> Spock is experimental — more optimizations are still to come.

All Spock times are the **mean of 150 runs** via `spock --benchmark`. Python, Node.js and LuaJIT times are measured with [hyperfine](https://github.com/sharkdp/hyperfine) (`--runs 150 --warmup 10`). All benchmarks are run on the same machine.

---

## Iterative Fibonacci - fib(40) × 200 000

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function main() {
    for _ in 0..200000 {
        let a = 0;
        let b = 1;
        let c = 0;
        for i in 0..39 {
            c = a + b;
            a = b;
            b = c;
        }
    }
}</code></pre></td>
<td><pre><code class="language-python">for _ in range(200000):
    a, b, c = 0, 1, 0
    for i in range(39):
        c = a + b
        a = b
        b = c</code></pre></td>
<td><pre><code class="language-javascript">for (let r = 0; r < 200000; r++) {
    let a = 0, b = 1, c = 0;
    for (let i = 0; i < 39; i++) {
        c = a + b;
        a = b;
        b = c;
    }
}</code></pre></td>
<td><pre><code class="language-lua">for r = 0, 199999 do
    local a, b, c = 0, 1, 0
    for i = 0, 38 do
        c = a + b
        a = b
        b = c
    end
end</code></pre></td>
</tr>
<tr>
  <td><b>93ms</b></td>
  <td>684ms</td>
  <td>35.8ms</td>
  <td>62.4ms</td>
</tr>
</table>

---

## Recursive Fibonacci - fib(30)

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function fib(n) {
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}

function main() {
    print(fib(30));
}</code></pre></td>
<td><pre><code class="language-python">def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

print(fib(30))</code></pre></td>
<td><pre><code class="language-javascript">function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

console.log(fib(30));</code></pre></td>
<td><pre><code class="language-lua">local function fib(n)
    if n <= 1 then return n end
    return fib(n - 1) + fib(n - 2)
end

print(fib(30))</code></pre></td>
</tr>
<tr>
  <td><b>35.9ms</b></td>
  <td>114.5ms</td>
  <td>45.6ms</td>
  <td>36.9ms</td>
</tr>
</table>

---

## Multiply, branch, modulo × 1 000 000

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function main() {
    let count = 0;
    let result = 1;
    while count < 1000000 {
        result *= 2;
        if result > 1000000 {
            result %= 1000000;
        }
        count += 1;
    }
    print(result);
}</code></pre></td>
<td><pre><code class="language-python">count = 0
result = 1
while count < 1000000:
    result *= 2
    if result > 1000000:
        result %= 1000000
    count += 1
print(result)</code></pre></td>
<td><pre><code class="language-javascript">let count = 0;
let result = 1;
while (count < 1000000) {
    result *= 2;
    if (result > 1000000) {
        result %= 1000000;
    }
    count++;
}
console.log(result);</code></pre></td>
<td><pre><code class="language-lua">local count = 0
local result = 1
while count < 1000000 do
    result = result * 2
    if result > 1000000 then
        result = result % 1000000
    end
    count = count + 1
end
print(result)</code></pre></td>
</tr>
<tr>
  <td><b>17.1ms</b></td>
  <td>137.8ms</td>
  <td>39.7ms</td>
  <td>25.6ms</td>
</tr>
</table>

---

## Sqrt × 10 000 000

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function main() {
    let x = 0.0;
    for i in 0..10000000 {
        x += float(i).sqrt();
    }
    print(x);
}</code></pre></td>
<td><pre><code class="language-python">from math import sqrt

x = 0.0
for i in range(10000000):
    x += sqrt(i)
print(x)</code></pre></td>
<td><pre><code class="language-javascript">let x = 0.0;
for (let i = 0; i < 10000000; i++) {
    x += Math.sqrt(i);
}
console.log(x);</code></pre></td>
<td><pre><code class="language-lua">local x = 0.0
for i = 0, 9999999 do
    x = x + math.sqrt(i)
end
print(x)</code></pre></td>
</tr>
<tr>
  <td><b>124.3ms</b></td>
  <td>992ms</td>
  <td>50ms</td>
  <td>173.8ms</td>
</tr>
</table>

---

## Sieve of Eratosthenes up to 100 000

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function main() {
    let limit = 100000;
    let sieve = range(limit);
    sieve[0] = 0;
    sieve[1] = 0;
    let i = 2;
    while i * i <= limit {
        if sieve[i] != 0 {
            let j = i * i;
            while j < limit {
                sieve[j] = 0;
                j += i;
            }
        }
        i += 1;
    }
    let count = 0;
    for x in sieve {
        if x != 0 { count += 1; }
    }
    print(count);
}</code></pre></td>
<td><pre><code class="language-python">limit = 100000
sieve = list(range(limit))
sieve[0] = 0
sieve[1] = 0
i = 2
while i * i <= limit:
    if sieve[i]:
        j = i * i
        while j < limit:
            sieve[j] = 0
            j += i
    i += 1
count = sum(1 for x in sieve if x)
print(count)</code></pre></td>
<td><pre><code class="language-javascript">const limit = 100000;
const sieve = Array.from(
    { length: limit }, (_, i) => i
);
sieve[0] = 0;
sieve[1] = 0;
for (let i = 2; i * i <= limit; i++) {
    if (sieve[i] !== 0) {
        for (let j = i*i; j < limit; j += i) {
            sieve[j] = 0;
        }
    }
}
console.log(sieve.filter(x => x > 0).length);</code></pre></td>
<td><pre><code class="language-lua">local limit = 100000
local sieve = {}
for i = 0, limit - 1 do
    sieve[i] = i
end
sieve[0] = 0
sieve[1] = 0
local i = 2
while i * i <= limit do
    if sieve[i] ~= 0 then
        local j = i * i
        while j < limit do
            sieve[j] = 0
            j = j + i
        end
    end
    i = i + 1
end
local count = 0
for _, v in pairs(sieve) do
    if v ~= 0 then count = count + 1 end
end
print(count)</code></pre></td>
</tr>
<tr>
  <td><b>3.1ms</b></td>
  <td>45.1ms</td>
  <td>39.8ms</td>
  <td>6.6ms</td>
</tr>
</table>

---

## String operations, array split and search × 50 000

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function main() {
    let s = "the quick brown fox";
    let count = 0;
    for _ in 0..50000 {
        let parts = s.split(" ");
        if parts.contains("fox") {
            count += 1;
        }
    }
    print(count);
}</code></pre></td>
<td><pre><code class="language-python">s = "the quick brown fox"
count = 0
for _ in range(50000):
    parts = s.split(" ")
    if "fox" in parts:
        count += 1
print(count)</code></pre></td>
<td><pre><code class="language-javascript">const s = "the quick brown fox";
let count = 0;
for (let i = 0; i < 50000; i++) {
    const parts = s.split(" ");
    if (parts.includes("fox")) {
        count++;
    }
}
console.log(count);</code></pre></td>
<td><pre><code class="language-lua">local s = "the quick brown fox"
local count = 0
for _ = 1, 50000 do
    if s:find("fox") then
        count = count + 1
    end
end
print(count)</code></pre></td>
</tr>
<tr>
  <td><b>3.5ms</b></td>
  <td>32.2ms</td>
  <td>37.5ms</td>
  <td>4.3ms</td>
</tr>
</table>

---

## FizzBuzz - 1 000 000 iterations

<table>
<tr>
  <th>Spock</th>
  <th>Python 3</th>
  <th>Node.js</th>
  <th>LuaJIT (-joff)</th>
</tr>
<tr>
<td><pre><code class="language-rust">function main() {
    let last = "";
    for i in 1..1000001 {
        if i % 15 == 0 {
            last = "FizzBuzz";
        } else if i % 3 == 0 {
            last = "Fizz";
        } else if i % 5 == 0 {
            last = "Buzz";
        } else {
            last = str(i);
        }
    }
    print(last);
}</code></pre></td>
<td><pre><code class="language-python">last = ""
for i in range(1, 1000001):
    if i % 15 == 0:
        last = "FizzBuzz"
    elif i % 3 == 0:
        last = "Fizz"
    elif i % 5 == 0:
        last = "Buzz"
    else:
        last = str(i)
print(last)</code></pre></td>
<td><pre><code class="language-javascript">let last = "";
for (let i = 1; i <= 1000000; i++) {
    if (i % 15 === 0) last = "FizzBuzz";
    else if (i % 3 === 0) last = "Fizz";
    else if (i % 5 === 0) last = "Buzz";
    else last = String(i);
}
console.log(last);</code></pre></td>
<td><pre><code class="language-lua">local last = ""
for i = 1, 1000000 do
    if i % 15 == 0 then
        last = "FizzBuzz"
    elseif i % 3 == 0 then
        last = "Fizz"
    elseif i % 5 == 0 then
        last = "Buzz"
    else
        last = tostring(i)
    end
end
print(last)</code></pre></td>
</tr>
<tr>
  <td><b>27.9ms</b></td>
  <td>172.5ms</td>
  <td>48.9ms</td>
  <td>84.3ms</td>
</tr>
</table>