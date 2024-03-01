return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      local gruvbox = require "gruvbox"

      gruvbox.setup {
        italic = {
          strings = false,
          comments = true,
          operators = false,
          folds = true,
        },
        overrides = {
          GruvboxHighlight = { bg = gruvbox.palette.dark3 },
          LspReferenceRead = { link = "GruvboxHighlight" },
          LspReferenceText = { link = "GruvboxHighlight" },
          LspReferenceWrite = { link = "GruvboxHighlight" },
          StatusLine = { fg = gruvbox.palette.light1, bg = gruvbox.palette.dark2 },
        },
      }
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    opts = function(_, opts)
      local cmp = require "cmp"

      opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.confirm { behavior = cmp.ConfirmBehavior.Insert, select = true }
        else
          fallback()
        end
      end, {
        "i",
        "s",
      })

      opts.sources = cmp.config.sources {
        { name = "nvim_lsp", priority = 1000 },
        { name = "buffer", priority = 750 },
        { name = "luasnip", priority = 500 },
        { name = "path", priority = 250 },
      }

      return opts
    end,
  },
  {
    "rebelot/heirline.nvim",
    opts = function(_, opts)
      local status = require "astronvim.utils.status"

      opts.statusline[4] = status.component.file_info { filename = { modify = ":p" } }
      opts.winbar = nil
      opts.tabline = nil

      return opts
    end,
  },
  {
    "tenxsoydev/size-matters.nvim",
    lazy = false,
    opts = {
      default_mappings = true,
      reset_font = vim.api.nvim_get_option "guifont",
      notifications = {
        enable = false,
      },
    },
  },
  {
    "mbbill/undotree",
    lazy = false,
  },
  "mfussenegger/nvim-jdtls", -- load jdtls on module
  {
    "williamboman/mason-lspconfig.nvim",
    opts = {
      ensure_installed = { "jdtls" },
    },
  },
  {
    "matze/vim-move",
    lazy = false,
  },
  {
    "kelly-lin/ranger.nvim",
    lazy = false,
    config = function() require("ranger-nvim").setup { replace_netrw = true } end,
  },
}
