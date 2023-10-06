return {
	"nvim-lualine/lualine.nvim",
	lazy = false,
	dependencies = { "nvim-tree/nvim-web-devicons" },
	theme = "gruvbox-light",
	opts = function()
		return { theme = "gruvbox-material" }
	end,
}
