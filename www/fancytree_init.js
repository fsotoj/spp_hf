// fancytree_vars_data for map tool

$(document).ready(function () {
  Shiny.addCustomMessageHandler('fancytree_vars_data', function (message) {
    const $tree = $("#fancytree_vars_demo");
    if ($tree.fancytree("instance")) $tree.fancytree("destroy");

    $tree.fancytree({
      source: message.data,
      icon: false,
      checkbox: false,
      selectMode: 1,           // single selection
      clickFolderMode: 2,      // click folders to expand, not select
      toggleEffect: false, // remove animation
      activate: function (event, data) {
        if (data.node && !data.node.folder) {
          // send a single key (no JSON array needed unless you want it)
          Shiny.setInputValue("selected_nodes_vars2", data.node.key, { priority: "event" });
        }
      }
    });

    // Select default, if provided
    const tree = $.ui.fancytree.getTree($tree);
    if (message.default_selected && message.default_selected.length) {
      const key = message.default_selected[0];
      const node = tree.getNodeByKey(key);
      if (node) {
        //console.log("STEP 1: Activating node:", node.key);
      
        node.makeVisible({ scrollIntoView: true });
        node.setActive();
      
        setTimeout(() => {
  const $box = $("#fancytree_vars_demo");
  const $el  = $(node.span);

  if ($el.length === 0) return;

  const elTop = $el.position().top;
  const offset = 130;   // ≈ 2 lines

  // ✅ Scroll directly to: element top - offset
  const newScroll = elTop - offset;

//  console.log("Scrolling unconditionally to", newScroll);

  $box.scrollTop(newScroll);

}, 400);

      }

    }
  });
});



// fancy tree vars for graph tool

Shiny.addCustomMessageHandler('fancytree_vars_data_graph', function (message) {
    const $tree = $("#fancytree_vars_demo_graph");

    // Destroy previous instance if exists
    if ($tree.fancytree("instance")) {
        $tree.fancytree("destroy");
    }

    // Initialize Fancytree
    $tree.fancytree({
        source: message.data,
        icon: false,
        checkbox: false,
        selectMode: 1,          // single selection
        clickFolderMode: 2,     // click = expand, not select
        toggleEffect: false, // remove animation

        activate: function (event, data) {
            // block selection of folders (datasets)
            if (data.node.folder) {
                data.node.deactivate();  // unselect folder
                data.node.toggleExpanded();
                return;
            }

            // ✅ send plain string key
            Shiny.setInputValue(
                "selected_nodes_vars_graph2",
                data.node.key,
                { priority: "event" }
            );
        }
    });

    // Apply default selected node (optional)
    const tree = $.ui.fancytree.getTree($tree);


    if (message.default_selected && message.default_selected.length > 0) {
        const key = message.default_selected[0];
        const node = tree.getNodeByKey(key);

        if (node) {
            node.makeVisible({ scrollIntoView: true });
            node.setActive();
        }
    }
});




/// state selector

Shiny.addCustomMessageHandler("fancytree_states_data", function (message) {
  const $tree = $("#fancytree_states_demo");

  if ($tree.fancytree("instance")) $tree.fancytree("destroy");

  $tree.fancytree({
    source: message.data,
    checkbox: true,
    selectMode: 3,
    icon: false,
    clickFolderMode: 2,
    toggleEffect: false, // remove animation

    select: function(event, data) {
      const tree = data.tree;
      const selected = tree.getSelectedNodes().map(n => n.key);
      Shiny.setInputValue("selected_nodes_states", JSON.stringify(selected), {priority: "event"});
    }
  });

  // ✅ Apply default selection
  const tree = $.ui.fancytree.getTree($tree);


  if (message.default_selected && message.default_selected.length) {
    message.default_selected.forEach(key => {
      const node = tree.getNodeByKey(key);
      if (node) node.setSelected(true);
    });
  }

  // ✅ Send initial selection
  const selectedKeys = tree.getSelectedNodes().map(n => n.key);
  Shiny.setInputValue("selected_nodes_states", JSON.stringify(selectedKeys));
});
