
function buildSourcesIndex()
{
    data = new XMLHttpRequest();
    data.open('GET', 'sources.json', false);
    data.send(null);
    sources = JSON.parse(data.response);

    toc = document.getElementById('toc');
    list = document.createElement('ul');
    toc.appendChild(list);

    for (index = 0; index < sources.length; index++)
    {
        source = sources[index];

        item = document.createElement('li');
        href = document.createElement('a');
        href.setAttribute('href',
                          'javascript:displaySource(\'' + source.file + '\')');
        text = document.createTextNode(source.file);
        href.appendChild(text);
        item.appendChild(href);
        list.appendChild(item);
    }
}

function displaySource(file)
{
    pane = document.getElementById('pane');

    while (pane.children.length != 0)
    {
        pane.removeChild(pane.children[0]);
    }

    data = new XMLHttpRequest();
    data.open('GET', file + '.json', false);
    data.send(null);
    source = JSON.parse(data.response);

    table = document.createElement('table');
    code = document.createElement('tbody');

    for (lineIndex = 0; lineIndex < source.length; lineIndex++)
    {
        line = source[lineIndex];
        row = document.createElement('tr');
        num = document.createElement('td');
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

function onload() {
    //  Load list of source files.
    buildSourcesIndex();
}
