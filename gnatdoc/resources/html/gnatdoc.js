
function buildText(root, data)
{
    var element;

    for (var index = 0; index < data.length; index++)
    {
        switch (data[index].kind)
        {
            case 'code':
                element = document.createElement('table');
                element.setAttribute('class', 'code');
                var code = document.createElement('tbody');

                for (lineIndex = 0;
                     lineIndex < data[index].children.length;
                     lineIndex++)
                {
                    var line = data[index].children[lineIndex];
                    var row = document.createElement('tr');
                    var cell = document.createElement('th');
                    cell.appendChild(document.createTextNode(line.number));
                    row.appendChild(cell);
                    cell = document.createElement('td');
                    buildText(cell, line.children);
                    row.appendChild(cell);
                    code.appendChild(row);
                }

                element.appendChild(code);
                break;

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

    /* Build 'Summary' section */

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

    /* Build 'Entities' section */

    header = document.createElement('h2');
    text = document.createTextNode('Entities');
    header.appendChild(text);
    pane.appendChild(header);

    for (var index = 0; index < GNATdocDocumentation.entities.length; index++)
    {
        var entity_set = GNATdocDocumentation.entities[index];
        var table;
        var tbody;

        header = document.createElement('h3');
        text = document.createTextNode(entity_set.label);
        header.appendChild(text);
        pane.appendChild(header);

        table = document.createElement('table');
        tbody = document.createElement('tbody');

        for (var eindex = 0; eindex < entity_set.entities.length; eindex++)
        {
            var entity = entity_set.entities[eindex];
            var row;
            var cell;

            row = document.createElement('tr');
            cell = document.createElement('td');
            cell.appendChild(document.createTextNode(entity.label));
            row.appendChild(cell);
            cell = document.createElement('td');
            buildText(cell, entity.summary);
            row.appendChild(cell);
            tbody.appendChild(row);
        }

        table.appendChild(tbody);
        pane.appendChild(table);
    }

    /* Build 'Description' section */

    header = document.createElement('h2');
    header.setAttribute('id', 'Description');
    text = document.createTextNode('Description');
    header.appendChild(text);
    pane.appendChild(header);
    buildText(pane, GNATdocDocumentation.description);

    /* Build entities description sections */

    for (var index = 0; index < GNATdocDocumentation.entities.length; index++)
    {
        var entity_set = GNATdocDocumentation.entities[index];

        for (var eindex = 0; eindex < entity_set.entities.length; eindex++)
        {
            var entity = entity_set.entities[eindex];

            header = document.createElement('h3');
            text = document.createTextNode(entity.label);
            header.appendChild(text);
            pane.appendChild(header);
            buildText(pane, entity.description);
        }
    }
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
    buildText(pane, [GNATdocSourceFile]);
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
