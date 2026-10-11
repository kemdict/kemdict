<script lang="ts">
  /*
   * Like Out.astro but usable in Svelte.
   *
   * klass: classes for the anchor element
   * id: id of the anchor element
   * href: where the link goes
   */
  let {
    href,
    title,
    class: klass = "link",
    id,
    children,
  }: {
    href?: string;
    title?: string;
    class?: string;
    id?: string;
    children: any;
  } = $props();
</script>

<a {href} {id} {title} class={klass} target="_blank" rel="noreferrer"
  >{@render children()}</a
>
