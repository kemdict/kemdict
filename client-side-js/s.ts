/**
 * @file Implementation for the searchBar element.
 * @name s.ts
 */
// Using let here will *sometimes* make TypeScript complain about
// redeclaring a block scoped variable. ...sure.
var searchForm = document.getElementById("searchForm") as HTMLElement;
var resultsList = document.getElementById("sr") as HTMLUListElement;

let titles: false | string[] = false;
function loadTitles(cb: (titles: string[]) => void) {
  if (titles) {
    cb(titles as string[]);
  } else {
    let xhr = new XMLHttpRequest();
    xhr.open("GET", "./titles.json", true);
    xhr.responseType = "json";
    xhr.addEventListener("loadend", (_event) => {
      if (xhr.status === 200) {
        titles = xhr.response;
        cb(titles as string[]);
      }
    });
    xhr.send();
  }
}
/**
 * Update the list of search results to show those matching `needle`.
 * @param needle
 */
function updateSearch(needle: string) {
  while (resultsList.firstChild) {
    resultsList.removeChild(resultsList.firstChild);
  }
  resultsList.textContent = "載入中…";
  if (needle.length > 0) {
    loadTitles((titles) => {
      while (resultsList.firstChild) {
        resultsList.removeChild(resultsList.firstChild);
      }
      let matching = titles.filter((title) => {
        return title.startsWith(needle);
      });
      matching.forEach((title) => {
        let c = document.createElement("li");
        let a = document.createElement("a");
        a.href = `/word/${title}`;
        a.textContent = title;
        c.appendChild(a);
        resultsList.appendChild(c);
      });
    });
  }
}

if (searchForm) {
  let searchBar = document.getElementById("searchBar") as HTMLInputElement;
  searchBar.addEventListener("change", (_event) => {
    updateSearch(searchBar.value);
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
