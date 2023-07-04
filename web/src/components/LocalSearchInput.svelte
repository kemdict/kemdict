<script lang="ts">
  /**
   * The current value of the input element, more reliable than just
   * setting bind:input.
   */
  export let filterString = "";

  // This is more reliable than binding value then setting input to
  // value in handlers. The bound value might update too late.
  let inputElem: HTMLInputElement;
  // Paused on compositionstart (IME starts editing), unpaused on
  // compositionend. This allows the user to continue seeing the
  // unfiltered list while still typing in their IME.
  let updatePaused = false;
</script>

<input
  type="search"
  autocomplete="off"
  placeholder="搜尋…"
  class="k-input mb-1 h-10 w-full"
  bind:this={inputElem}
  on:input={() => {
    if (inputElem.value === "" || !updatePaused) {
      filterString = inputElem.value;
    }
  }}
  on:compositionstart={() => (updatePaused = true)}
  on:compositionend={() => (updatePaused = false)}
/>
