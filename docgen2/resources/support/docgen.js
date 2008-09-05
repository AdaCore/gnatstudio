myGetAttribute = function (elt, attribute) {
    if (attribute == "class") attribute = "className";
    return elt[attribute];
}

mySetAttribute = function (elt, attribute, value) {
    if (attribute == "class") attribute = "className";
    elt[attribute] = value;
}

function setHidingLinks() {
  onloadDoc();
 }

function onloadDoc() {
  if (!document.getElementsByTagName) { return; }
  if (!document.getElementById) { return; }
  var main = document.getElementById ('leftSideInside');
  var link1, link2;

  setLinksForTag ('h2', '');
  link1 = setLinksForTag ('h3', '');
  link2 = setLinksForTag ('h4', 'hidden');

  if (link1 | link2) {
    div=document.createElement('div');
    main.appendChild (div);

    div=document.createElement('div');
    div.setAttribute ('class', 'button');
    div.setAttribute ('className', 'button'); /* for Internet Explorer */
    text=document.createTextNode('Unfold');
    div.appendChild (text);
    lnk=document.createElement ("a");
    lnk.setAttribute ('href', '#');
    lnk.onclick = function () {showAllTags ('h3'); return false;}
    lnk.appendChild (div);
    main.appendChild (lnk);

    var h4s = document.getElementsByTagName ('h4');
    if (h4s.length > 0) {
      div=document.createElement('div');
      div.setAttribute ('class', 'button');
      div.setAttribute ('className', 'button'); /* for Internet Explorer */
      text=document.createTextNode('Unfold all');
      div.appendChild (text);
      lnk=document.createElement ("a");
      lnk.setAttribute ('href', '#');
      lnk.onclick = function () {showAllTags ('h3'); showAllTags ('h4'); return false;}
      lnk.appendChild (div);
      main.appendChild (lnk);
    }

    div=document.createElement('div');
    div.setAttribute ('class', 'button');
    div.setAttribute ('className', 'button'); /* for Internet Explorer */
    text=document.createTextNode('Fold all');
    div.appendChild (text);
    lnk=document.createElement ("a");
    lnk.setAttribute ('href', '#');
    lnk.onclick = function () {hideAllTags ('h4'); hideAllTags ('h3'); return false;}
    lnk.appendChild (div);
    main.appendChild (lnk);
  }

  tags=document.getElementsByTagName('a');
  for (var j=0; j<tags.length; j++) {
    var href=tags[j].getAttribute ('href');
    if (href == null) continue;
    if ((href != '#') && (href.indexOf('#') > -1)) {
      tags[j].onclick = function() {showLocationFromA(this); return true}
    }
  }

  //  Should open a referenced entity
  ref = document.location.hash;
  if (ref != '') {
    if (ref.indexOf('#') > -1) {
      ref = ref.substr (1);
      showLocation (ref);
      elem = document.getElementById (ref);
      if (elem) window.scrollTo (0,elem.offsetTop);

    }
  }
}

function sortElem(a,b)
{
  return a[0].toLowerCase() > b[0].toLowerCase() ? 1 : -1;
}

function printIndexList (names) {
  if (names.length > 1) {
    names.sort (sortElem);
    document.write ('<ul>');
    for (var j = 0; j < names.length; j++) {
      if (names[j][0] != 'dummy') {
        document.write ('<li class="'+names[j][2]+'"><a href="#'+names[j][1]+'" onclick="showLocation(\''+names[j][1]+'\')">'+names[j][0]+'</a></li>');
      }
    }
    document.write ('</ul>');
  }
}

function setLinksForTag (tag, defaultstate) {
  var titles = document.getElementsByTagName (tag);

  for (var i=0; i<titles.length; i++){
    if (canToggle (titles[i])) {
      // insert <a href="#" onclick="return hide(this)">&#9660;</a>
      text=document.createTextNode('▼');
      lnk=document.createElement("a");
      lnk.setAttribute ('href', '#');
      lnk.setAttribute ('class', 'hide');
      lnk.setAttribute ('className', 'hide'); /* for Internet Explorer */
      lnk.appendChild(text);
      lnk.onclick = function () {toggle(this); return false;}
      titles[i].insertBefore (lnk, titles[i].firstChild);
      if (defaultstate == 'hidden')
        toggle (lnk);
    }
  }
  return titles.length > 0;
}
function showLocationFromA (tag) {
  var href = tag.getAttribute('href');
  url = href.substr (0, href.indexOf('#'));
  while (url.indexOf('/') > -1) url = url.substr (url.indexOf('/') + 1);
  myurl = document.location.pathname;
  while (myurl.indexOf('/') > -1) myurl = myurl.substr (myurl.indexOf('/') + 1);
  if ((url != myurl) && (url != '')) return true;
  href = href.substr (href.indexOf('#') + 1);
  showLocation (href);
}

function showLocation (loc) {
  var links = document.getElementsByTagName('a');
  for (var j=0; j < links.length; j++) {
    if (links[j].getAttribute ('name') == loc) {
      var parent = links[j].parentNode;
      while (parent != document) {
        var elem = parent.firstChild;
        if (elem.firstChild != null)
          if (elem.firstChild.nodeValue == '►')
            toggle (elem);
        if (parent.tagName.toLowerCase() == 'div') {
          for (var k=0; k < parent.childNodes.length; k++) {
            if (parent.childNodes[k].nodeType == 1 &&
                parent.childNodes[k].firstChild != null &&
                parent.childNodes[k].firstChild.nodeType == 1 &&
                parent.childNodes[k].firstChild.tagName.toLowerCase() == 'a') {
              elem = parent.childNodes[k].firstChild;
              if (elem.firstChild != null)
                if (elem.firstChild.nodeValue == '►')
                  toggle (elem);
            }
          }
        }
        parent = parent.parentNode;
      }
      return;
    }
  }
  return;
}

function hideAllTags (tag) {
  if (!document.getElementsByTagName) { return; }
  var titles = document.getElementsByTagName (tag);
  for (var i=0; i<titles.length; i++){
    if (titles[i].firstChild.firstChild.nodeValue == '▼') {
      toggle (titles[i].firstChild);
    }
  }
}

function showAllTags (tag) {
  if (!document.getElementsByTagName) { return; }
  var titles = document.getElementsByTagName (tag);
  for (var i=0; i<titles.length; i++){
    if (titles[i].firstChild.firstChild.nodeValue == '►') {
      toggle (titles[i].firstChild);
    }
  }
}

function canToggle (elem) {
  var children = elem.parentNode.childNodes;

  for (var j = 0; j < children.length; j++) {
    var el = children[j];
    if (el.nodeType == 1 &&
        el.tagName.toLowerCase() != 'script' &&
        el.tagName.toLowerCase() != 'h2' &&
        el.tagName.toLowerCase() != 'h3' &&
        el.tagName.toLowerCase() != 'h4') {
      return true;
    }
  }
  return false;
}

function toggle (elem) {
  var children = elem.parentNode.parentNode.childNodes;
  var next_display;

  if (elem.childNodes[0].nodeValue == '▼') {
    next_display = 'none';
    elem.childNodes[0].nodeValue = '►';
  } else {
    next_display = 'block';
    elem.childNodes[0].nodeValue = '▼';
  }
  for (var j = 0; j < children.length; j++) {
    var el = children[j];
    if (el.nodeType == 1 &&
        el.tagName.toLowerCase() != 'script' &&
        el.tagName.toLowerCase() != 'h2' &&
        el.tagName.toLowerCase() != 'h3' &&
        el.tagName.toLowerCase() != 'h4') {
      el.style.display = next_display;
    }
  }
  return false;
}
