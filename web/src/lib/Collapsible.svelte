<script>
  import collapse from "svelte-collapse";

  let klass = undefined;
  export { klass as class };
  export let open = true;
  let duration = 0.1;
  let easing = "ease";

  function handleToggle() {
    open = !open;
  }
</script>

<div aria-expanded={open}>
  <h2
    class:open
    class={klass}
    on:click={handleToggle}
    on:keypress={handleToggle}
  >
    <slot name="header" />
  </h2>
  <div use:collapse={{ open, duration, easing }}>
    <slot name="body" />
  </div>
</div>

<style>
  h2 {
    cursor: pointer;
    user-select: none;
  }
  h2 {
    position: relative;
    flex-grow: 1;
    padding-left: 1rem;
    text-align: left;
  }
  h2.open:before {
    transform: translate(0, -100%) rotate(45deg);
  }
  h2:before {
    transform: translate(0, -100%) rotate(-135deg);
    position: absolute;
    bottom: 4px;
    left: 0;
    display: block;
    height: 0.5rem;
    width: 0.5rem;
    content: "";
    transform-origin: 75% 75%;
    box-shadow: 1px 1px;
    pointer-events: none;
  }
</style>
