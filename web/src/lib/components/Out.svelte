<script>
  /* An outgoing link.
     This lets me avoid specifying target and rel over and over again.

     klass: classes for the anchor element
     id: id of the anchor element
     href: where the link goes
   */
  export let href;
  export let klass;
  export let id;
</script>

<a {href} {id} class={klass} target="_blank" rel="noreferrer"><slot /></a>

<style>
  a:after {
    content: "°";
  }
</style>
