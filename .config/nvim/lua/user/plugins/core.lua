return {
  {
    "goolord/alpha-nvim",
    enabled = false,
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    opts = function(_, opts) opts.filesystem.hijack_netrw_behavior = "disabled" end,
  },
}
