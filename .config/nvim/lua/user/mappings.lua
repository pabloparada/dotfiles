return {
  n = {
    ["<leader>e"] = false,
    ["<leader>"] = {
      ["b"] = { name = require("astronvim.utils").get_icon("Tab", 1, true) .. "Buffers" },
      ["e"] = { name = require("astronvim.utils").get_icon("Debugger", 1, true) .. "Debugger" },
    },
    ["<leader>bn"] = { "<cmd>tabnew<cr>", desc = "New Tab" },
    ["<leader>bv"] = { "<cmd>vsplit<cr>", desc = "Vertical Split" },
    ["<leader>bh"] = { "<cmd>split<cr>", desc = "Horizontal Split" },
    ["<leader>pp"] = { "<cmd>:Telescope projects<cr>", desc = "Projects" },
    ["<leader>fs"] = { "<cmd>:w", desc = "Save Buffer" },
    ["J"] = { "mzJ`z" },
    ["<C-d>"] = { "<C-d>zz" },
    ["<C-u>"] = { "<C-u>zz" },
    ["<n>"] = { "nzzzv" },
    ["<N>"] = { "Nzzzv" },
    ["<leader>d"] = { '"_d', desc = "Delete to Black Hole Register" },
    ["Q"] = { "<nop>" },
    ["<leader><leader>r"] = { name = "Astro Config" },
    ["<leader><leader>rc"] = { ":AstroReload<cr>", desc = "Reloads config" },
    ["<leader>bb"] = false,
    ["<leader>bd"] = false,
    ["<leader>b\\"] = false,
    ["<leader>b|"] = false,
    ["<C-+>"] = { "<cmd>:FontSizeUp<cr>" },
    ["<C-->"] = { "<cmd>:FontSizeDown<cr>" },
    ["<C-=>"] = { "<cmd>:FontReset<cr>" },
    ["<F5>"] = { "<cmd>:UndotreeToggle<cr>" },
    ["<leader>eb"] = { function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint (F9)" },
    ["<leader>eB"] = { function() require("dap").clear_breakpoints() end, desc = "Clear Breakpoints" },
    ["<leader>ec"] = { function() require("dap").continue() end, desc = "Start/Continue (F5)" },
    ["<leader>eC"] = {
      function()
        vim.ui.input({ prompt = "Condition: " }, function(condition)
          if condition then require("dap").set_breakpoint(condition) end
        end)
      end,
      desc = "Conditional Breakpoint (S-F9)",
    },
    ["<leader>ei"] = { function() require("dap").step_into() end, desc = "Step Into (F11)" },
    ["<leader>eo"] = { function() require("dap").step_over() end, desc = "Step Over (F10)" },
    ["<leader>eO"] = { function() require("dap").step_out() end, desc = "Step Out (S-F11)" },
    ["<leader>eq"] = { function() require("dap").close() end, desc = "Close Session" },
    ["<leader>eQ"] = { function() require("dap").terminate() end, desc = "Terminate Session (S-F5)" },
    ["<leader>ep"] = { function() require("dap").pause() end, desc = "Pause (F6)" },
    ["<leader>er"] = { function() require("dap").restart_frame() end, desc = "Restart (C-F5)" },
    ["<leader>eR"] = { function() require("dap").repl.toggle() end, desc = "Toggle REPL" },
    ["<leader>es"] = { function() require("dap").run_to_cursor() end, desc = "Run To Cursor" },
    ["<leader>eE"] = {
      function()
        vim.ui.input({ prompt = "Expression: " }, function(expr)
          if expr then require("dapui").eval(expr) end
        end)
      end,
      desc = "Evaluate Input",
    },
    ["<leader>eu"] = { function() require("dapui").toggle() end, desc = "Toggle Debugger UI" },
    ["<leader>eh"] = { function() require("dap.ui.widgets").hover() end, desc = "Debugger Hover" },
    ["K"] = { function() require("hover").hover() end, desc = "Show Documentation" },
    ["gK"] = { function() require("hover").hover_select() end, desc = "Jump to Documentation" },
    ["<leader>gg"] = { "<cmd>:Neogit<cr>", desc = "Neogit" },
  },
  v = {
    ["<leader>e"] = { name = "Debugger" },
    ["<leader>d"] = { '"_d', desc = "Delete to Black Hole Register" },
    ["<leader>eE"] = { function() require("dapui").eval() end, desc = "Evaluate Input" },
  },
  t = {},
  x = {
    ["<leader>e"] = { name = "Debugger" },
    ["<leader>p"] = { '"_dP' },
  },
}
