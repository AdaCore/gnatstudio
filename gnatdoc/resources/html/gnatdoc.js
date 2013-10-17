
function buildText(root, data)
{
    var element;

    for (var index = 0; index < data.length; index++)
    {
        switch (data[index].kind)
        {
            case 'paragraph':
                element = document.createElement('p');
                buildText(element, data[index].children);
                break;

            case 'span':
                element = document.createElement('span');
                element.appendChild(document.createTextNode(data[index].text));

                if (typeof data[index].class !== 'undefined')
                {
                    element.setAttribute('class', data[index].class);
                }

                break;
        }
        root.appendChild(element);
    }
}

function buildDocumentationPage()
{
    var pane;
    var header;
    var text;
    var href;

    pane = document.getElementById('body');

    header = document.createElement('h2');
    text = document.createTextNode('Summary');
    header.appendChild(text);
    pane.appendChild(header);
    buildText(pane, GNATdocDocumentation.summary);
    href = document.createElement('a');
    href.setAttribute('href', '#Description');
    text = document.createTextNode(' More...');
    href.appendChild(text);
    pane.appendChild(href);

    header = document.createElement('h2');
    header.setAttribute('id', 'Description');
    text = document.createTextNode('Description');
    header.appendChild(text);
    pane.appendChild(header);
    buildText(pane, GNATdocDocumentation.description);
}

function buildPackagesIndex(toc)
{
    header = document.createElement('h1');
    text = document.createTextNode('Packages and Classes');
    header.appendChild(text);
    toc.appendChild(header);

    list = document.createElement('ul');
    toc.appendChild(list);

    for (index = 0; index < GNATdocDocumentationIndex.length; index++)
    {
        entry = GNATdocDocumentationIndex[index];

        item = document.createElement('li');
        href = document.createElement('a');
        href.setAttribute('href', entry.file);
        href.setAttribute('target', 'contentView');
        text = document.createTextNode(entry.label);
        href.appendChild(text);
        item.appendChild(href);
        list.appendChild(item);
    }
}

function buildSourcesIndex(toc)
{
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

function onDocumentationLoad()
{
    buildDocumentationPage();
}

function onLoad()
{
    toc = document.getElementById('tocView');
    buildPackagesIndex(toc);
    buildSourcesIndex(toc);
}

function onSourceFileLoad()
{
    displaySource();
}
