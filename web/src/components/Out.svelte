<script>
  import clsx from "clsx";
  /* Like Out.astro but usable in Svelte.

     klass: classes for the anchor element
     id: id of the anchor element
     href: where the link goes
   */
  export let href = undefined;
  export let after = true;
  export let title = undefined;
  let klass = "link";
  export { klass as class };
  export let id = undefined;
</script>

<a
  {href}
  {id}
  {title}
  class={clsx(after && "after:content-['°']", klass)}
  target="_blank"
  rel="noreferrer"><slot /></a
>
