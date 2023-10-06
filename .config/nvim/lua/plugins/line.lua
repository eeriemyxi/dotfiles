return {
	"nvim-lualine/lualine.nvim",
	event = "VeryLazy",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	theme = "gruvbox-light",
	opts = function()
		return { theme = "gruvbox-material" }
	end,
}
