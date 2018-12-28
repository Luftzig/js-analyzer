const esprima = require("esprima")

process.stdin.resume()
process.stdin.setEncoding("utf8")
const data = () => {
  const lines = []
  return {
    get: () => lines,
    append: (line) => {
      lines.push(line);
      return lines
    }
  }
}
const {append: appendData, get: getData} = data()
process.stdin.on("data", appendData)
process.stdin.on("end", () => {
  const result = esprima.parseModule(getData().join("\n"), {jsx: true, tolerant: true})
  process.stdout.write(JSON.stringify(result), () => process.exit(0))
})
