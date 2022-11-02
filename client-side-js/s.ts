/**
 * @file Implementation for the searchBar element.
 * @name s.ts
 */
// Using let here will *sometimes* make TypeScript complain about
// redeclaring a block scoped variable. ...sure.
var searchForm = document.getElementById("sf") as HTMLElement;
var resultsList = document.getElementById("sr") as HTMLUListElement;
var sbc = document.getElementById("sbc") as HTMLElement;

let loading = false;
let titles: false | string[] = false;
function loadTitles(cb: (titles: string[]) => void) {
  if (titles) {
    cb(titles as string[]);
  } else if (loading) {
    // A load is already happening. Don't do another.
    return;
  } else {
    loading = true;
    let xhr = new XMLHttpRequest();
    xhr.open("GET", "/titles.json", true);
    xhr.responseType = "json";
    xhr.addEventListener("loadend", (_event) => {
      loading = false;
      if (xhr.status === 200) {
        titles = xhr.response;
        cb(titles as string[]);
      }
      // TODO: add a callback for failure
    });
    xhr.send();
  }
}

/**
 * If `hidden` is true, hide `elem`. Otherwise show it.
 */
function setHidden(elem: HTMLElement, hide: boolean) {
  if (hide) {
    elem.classList.remove("visible");
    elem.classList.add("invisible");
  } else {
    elem.classList.remove("invisible");
    elem.classList.add("visible");
  }
}

/**
 * A wrapper over string matching methods to call them more easily
 */
const Match = {
  includes: (str: string, needle: string) => str.indexOf(needle) === -1,
  startsWith: (str: string, needle: string) => str.startsWith(needle),
  endsWith: (str: string, needle: string) => str.endsWith(needle),
};

/**
 * Update the list of search results to show those matching `needle`.
 * @param needle
 */
function updateSearch(needle: string) {
  while (resultsList.firstChild) {
    resultsList.removeChild(resultsList.firstChild);
  }
  if (needle.length > 0) {
    setHidden(resultsList, false);
    let loading = document.createElement("p");
    loading.textContent = "載入中…";
    resultsList.appendChild(loading);
    // startsWith, endsWith, indexOf
    // TODO:
    const matchFunc = Match["startsWith"];
    loadTitles((titles) => {
      while (resultsList.firstChild) {
        resultsList.removeChild(resultsList.firstChild);
      }
      for (const title of titles) {
        if (matchFunc(title, needle)) {
          let c = document.createElement("li");
          let a = document.createElement("a");
          a.href = `/word/${title}`;
          a.textContent = title;
          c.appendChild(a);
          resultsList.appendChild(c);
        }
      }
      setHidden(resultsList, false);
    });
  }
}

if (searchForm) {
  let searchBar = document.getElementById("sb") as HTMLInputElement;
  searchBar.addEventListener("input", (_event) => {
    updateSearch(searchBar.value);
  });
  searchBar.addEventListener("focus", (_event) => {
    setHidden(resultsList, false);
  });
  searchForm.addEventListener("submit", (event) => {
    let word = searchBar.value;
    event.preventDefault();
    if (word.trim() !== "") {
      // HACK around the fact that we can't access Eleventy's
      // pathprefix from here.
      //
      // We could put this file in a template (like s.js.njk), except
      // then we'd have to specify the right path in the permalink
      // prop, as part of the front matter. The front matter syntax is
      // not valid JS and will cause editing tools to barf; it can be
      // customized, but that customization is global across the entire
      // project.
      // I'll just do this instead.
      if (location.href.indexOf("word") == -1) {
        location.href = "word/" + word;
      } else {
        location.href = "" + word;
      }
    }
  });
}

// Modal dismissal logic:
// https://stackoverflow.com/a/54441029/6927814
//
// Have a full-size parent element which, when clicked, hides the
// modal. Here we use the root element as the parent element.
// Then we get the modal itself (the search form's parent container)
// to block click events from going through to the root document.
// Now the search result list hides when we click away.
sbc.addEventListener("click", (event) => {
  // event.preventDefault();
  event.stopPropagation();
  event.stopImmediatePropagation();
  return false;
});

document.addEventListener("click", (_event) => {
  setHidden(resultsList, true);
});
