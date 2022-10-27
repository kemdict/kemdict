"use strict";

// Implementation for the searchBar element.

var searchForm;
if ((searchForm = document.getElementById("searchForm"))) {
  var searchBar = document.getElementById("searchBar");
  searchForm.addEventListener("submit", function (event) {
    var word = searchBar.value;
    event.preventDefault();
    if (word.trim() !== "") {
      location.href = "/word/" + word;
    }
  });
}
