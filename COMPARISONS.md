> Please note Spock is very experimental, and as such many more optimizations and speed improvements are still to come.

- ## Fib(40) repeated 200000 times

<table border="0">
 <tr>
    <td><b style="font-size:20px">Spock</b></td>
    <td><b style="font-size:20px">Python</b></td>
    <td><b style="font-size:20px">LuaJIT (-j off)</b></td>
 </tr>
 <tr>
    <td><pre><code>fn main() {
for _ in range(0,200000) {
let n = 40;
let a=0;
let b=1;
let c=0;
let i=0;
while i < n {
   c = a+b;
   a = b;
   b = c;
   i = i+1;
}
}
}</code></pre></td>
    <td><pre><code>for x in range(0,200000):
    n = 40
    a=0
    b=1
    c=0
    i=0
    while i < n:
        c = a+b
        a = b
        b = c
        i = i+1</code></pre></td>
<td><pre><code>for x = 0, 199999 do
    local n = 40
    local a = 0
    local b = 1
    local c = 0
    local i = 0
    while i < n do
        c = a + b
        a = b
        b = c
        i = i + 1
    end
end</code></pre></td>
 </tr>
<tr>
<td>
0.0023s (404x faster)
</td>
<td>
0.93s
</td>
<td>
0.0059s
</td>
</tr>
</table>

- ## Array looping and modification

<table border="0">
 <tr>
    <td><b style="font-size:20px">Spock</b></td>
    <td><b style="font-size:20px">Python</b></td>
 </tr>
 <tr>
    <td><pre><code>fn main() {
    let i = [0,1,2,3];
    for x in i.repeat(30000000) {
        i.push(x);
    }
    print(i.len());
}</code></pre></td>
    <td><pre><code>i = [0,1,2,3]
for x in i*30000000:
    i.append(x)
print(len(i))</code></pre></td>
 </tr>
<tr>
<td>
1.8s (9.6x faster)
</td>
<td>
17.4s
</td>
</tr>
</table>

- ## Range and square root

<table border="0">
 <tr>
    <td><b style="font-size:20px">Spock</b></td>
    <td><b style="font-size:20px">Python</b></td>
    <td><b style="font-size:20px">LuaJIT (-j off)</b></td>
 </tr>
 <tr>
    <td><pre><code>fn main() {
    let x = 0;
    for i in range(0,10000000) {
        x += i.sqrt();
    }
    print(x);
}</code></pre></td>
    <td><pre><code>from math import sqrt
x = 0
for i in range(0,10000000):
    x = x+sqrt(i)
print(x)</code></pre></td>
<td><pre><code>local x = 0
for i = 0, 9999999 do
    x = x + math.sqrt(i)
end
print(x)</code></pre></td>
</tr>
<tr>
<td>
0.199s (7x faster)
</td>
<td>
1.4s
</td>
<td>
0.019s
</td>
</tr>
</table>

- ## While loop, condition, modulo, multiplication and addition

<table border="0">
 <tr>
    <td><b style="font-size:20px">Spock</b></td>
    <td><b style="font-size:20px">Python</b></td>
    <td><b style="font-size:20px">LuaJIT (-j off)</b></td>
 </tr>
 <tr>
    <td><pre><code>fn main() {
let count = 0;
let limit = 1000000;
let result = 1;
while count < limit {
    result *= 2;
    if result > 1000000 {
        result %= 1000000;
    }
    count += 1;
}
print(result);
}</code></pre></td>
    <td><pre><code>count = 0
limit = 1000000
result = 1
while count < limit:
    result = result * 2;
    if result > 1000000:
        result = result % 1000000
    count += 1
print(result)</code></pre></td>
<td><pre><code>local count = 0
local limit = 1000000
local result = 1
while count < limit do
    result = result * 2
    if result > 1000000 then
        result = result % 1000000
    end
    count = count + 1
end
print(result)</code></pre></td>
</tr>
<tr>
<td>
0.018s (5.7x faster)
</td>
<td>
0.120s
</td>
<td>
0.014s
</td>
</tr>
</table>


<details>
<summary>Invalid benchmarks</summary>

### Basic loop sum

<table border="0">
 <tr>
    <td><b style="font-size:20px">Spock</b></td>
    <td><b style="font-size:20px">Python</b></td>
    <td><b style="font-size:20px">LuaJIT (-j off)</b></td>
 </tr>
 <tr>
    <td><pre><code>fn main() {
    let i = 0;
    while i < 999999999 {
        i += 1;
    }
    print(i);
}</code></pre></td>
    <td><pre><code>i = 0
while i < 999999999:
    i = i + 1
print(i)</code></pre></td>
<td><pre><code>local i = 0
while i < 999999999 do
    i = i + 1
end
print(i)</code></pre></td>
 </tr>
<tr>
<td>
0.000019s
</td>
<td>
42s
</td>
<td>
0.33s
</td>
</tr>
</table>


</details>
