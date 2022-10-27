"use strict";

// Implementation for the searchBar element.

var searchForm;
if ((searchForm = document.getElementById("searchForm"))) {
  var searchBar = document.getElementById("searchBar");
  searchForm.addEventListener("submit", function (event) {
    var word = searchBar.value;
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
        console.log("b");
      } else {
        location.href = "../" + word;
        console.log("a");
      }
    }
  });
}
