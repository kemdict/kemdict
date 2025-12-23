<script lang="ts">
  import clsx from "clsx";
  /*
   * Like Out.astro but usable in Svelte.
   *
   * klass: classes for the anchor element
   * id: id of the anchor element
   * href: where the link goes
   */
  let {
    href,
    after = true,
    title,
    class: klass = "link",
    id,
    children,
  }: {
    href?: string;
    after?: boolean;
    title?: string;
    class?: string;
    id?: string;
    children: any;
  } = $props();
</script>

<a
  {href}
  {id}
  {title}
  class={clsx(after && "after:content-['°']", klass)}
  target="_blank"
  rel="noreferrer">{@render children()}</a
>
