
function buildSourcesIndex()
{
    toc = document.getElementById('tocView');
    header = document.createElement('h1');
    text = document.createTextNode('Source Files');
    header.appendChild(text);
    toc.appendChild(header);
    list = document.createElement('ul');
    toc.appendChild(list);

    for (index = 0; index < GNATdocSourceFileIndex.length; index++)
    {
        source = GNATdocSourceFileIndex[index];

        item = document.createElement('li');
        href = document.createElement('a');
        href.setAttribute('href', source.file + '.html');
        href.setAttribute('target', 'contentView');
        text = document.createTextNode(source.file);
        href.appendChild(text);
        item.appendChild(href);
        list.appendChild(item);
    }
}

function displaySource()
{
    pane = document.getElementById('body');

    source = GNATdocSourceFile;

    table = document.createElement('table');
    table.setAttribute('class', 'code');
    code = document.createElement('tbody');

    for (lineIndex = 0; lineIndex < source.length; lineIndex++)
    {
        line = source[lineIndex];
        row = document.createElement('tr');
        num = document.createElement('th');
        spans = document.createElement('td');
        text = document.createTextNode((lineIndex + 1).toString());
        num.appendChild(text);

        for (spanIndex = 0; spanIndex < line.length; spanIndex++)
        {
            span = document.createElement('span');
            span.setAttribute('class', line[spanIndex].class);
            text = document.createTextNode(line[spanIndex].text);
            span.appendChild(text);
            spans.appendChild(span);
        }

        row.appendChild(num);
        row.appendChild(spans);
        code.appendChild(row);

    }

    table.appendChild(code);
    pane.appendChild(table);
}

function onLoad() {
    buildSourcesIndex();
}

function onSourceFileLoad() {
    displaySource();
}
