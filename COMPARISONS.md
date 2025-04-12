- ## Basic loop sum

<table border="0">
 <tr>
    <td><b style="font-size:20px">Spock code</b></td>
    <td><b style="font-size:20px">Python code</b></td>
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
 </tr>
<tr>
<td>
5.5s
</td>
<td>
42s
</td>
</tr>
</table>