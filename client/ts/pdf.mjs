import pdfCreatorNode from "pdf-creator-node"
import fs from "fs"

var html = fs.readFileSync("abc.html", "utf8");
const document = {
    html,
    data: {},
    path: "./output.pdf"
}

pdfCreatorNode.create(document)