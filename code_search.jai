#import "Basic";
#import "Window_Creation";
#import "Input";
#import "Simp";
#import "GetRect_LeftHanded";
#import "String";
#import "File";
#import "File_Utilities";
#import "Thread";
#import "Math";
#import "Hash_Table";
#import "Text_File_Handler";

DEBUG :: false;

window_width  : s32 = 1920;
window_height : s32 = 1080;

theme: Overall_Theme;

TheFont: *Dynamic_Font;
LoadFont :: () {
    TheFont = get_font_at_size("data", "ProggyVector-Regular.ttf", xx (window_height/64.0));
}

scroll_region_value: float;
scroll_region_value_for_file_contents: float;
scroll_region_value_for_search_dirs: float;

ExecuteQueryForFile :: (query: string, file_index: s64) {
	file: *LoadedFile = *Files[file_index];
    if !file.loaded {
        log("Loading file %", file.path);
        file.contents, file.loaded = read_entire_file(file.path);
        if !file.loaded then {
            log("Failed to load file %", file.path);
            return;
        }

        dup_ptr := file.contents;
        start_of_line_index := 0;
        end_of_line_index := 0;
        for index: 0..file.contents.count-1 {
        	char := file.contents[index];
        	if char == #char "\n" {
        		end_of_line_index := index;
        		line: Line;
        		line.index = start_of_line_index;
        		line.text = file.contents;
        		line.text.count = index+1;
        		advance(*line.text, start_of_line_index);
				// line.text = trim(line.text);

				// log("line %: %", file.lines.count+1, line.text);

        		array_add(*file.lines, line);

        		// "abcd1234\nsjdflaksdjf\n"
        		//            ^
        		//       start of line

        		start_of_line_index = index+1;
        	}
        }
    }
    if file.loaded {
    	for line: file.lines {
    		if contains(line.text, query) {
	    		query_result: QueryResult;
	            query_result.file_index = file_index;
	            query_result.line_index = it_index;
	            lock(*TheMutex);
	            array_add(*QueryResults, query_result);
	            unlock(*TheMutex);
    		}
    	}
    }
}

ExecuteQuery :: (query: string) {
    if query.count == 0 then return;

    lock(*QueryMutex);
    array_reset(*QueryResults);
    unlock(*QueryMutex);

    for file_index: 0..Files.count-1 {
        ExecuteQueryForFile(query, file_index);
    }
}

Line :: struct {
	index := -1;
	text: string;
}
LastQuery: string;
LoadedFile :: struct {
    name: string;
    path: string;
    contents: string;
    lines: [..]Line;
    loaded := false;
}
Files: [..]LoadedFile;

QueryResult :: struct {
    line_index := -1;
    file_index := -1;
}
QueryResults: [..]QueryResult;


TheMutex: Mutex;
QueryMutex: Mutex;
ExecutingQuery := false;

DirectoryState :: struct {
	path: string;
	scanned := false;
}

VaildFiletypes: [..]string;
DirectoriesToSearch: [..]DirectoryState;

ScanDirectory :: (directory: *DirectoryState) {
	log("Scanning directory: %", directory.path);

	DirectoryScanProgress :: struct {
		files_added := 0;
		files: *[..]LoadedFile;
	}
	progress: DirectoryScanProgress;
	progress.files = *Files;

	visitor :: (info: *File_Visit_Info, progress: *DirectoryScanProgress) {
        is_text_file := false;
        for filetype: VaildFiletypes {
            if ends_with(info.short_name, filetype) {
                is_text_file = true;
                break;
            }
        }
        if !is_text_file then return;
        if begins_with(info.short_name, ".") then return;

        loaded_file: LoadedFile;
        loaded_file.name = copy_string(info.short_name);
        loaded_file.path = copy_string(info.full_name);
        array_add(progress.files, loaded_file);
        progress.files_added += 1;
    }
	visit_files(directory.path, recursive=true, *progress, visitor, visit_directories=true);
	directory.scanned = true;

	log("Scan complete. Added % files", progress.files_added);
}

QueryThreadInitialized := false;
QueryThread: Thread;
QueryThreadProc :: (thread: *Thread) -> s64 {
	while true {
        lock(*TheMutex);
        query_updated := LastQuery != Query;
        if query_updated then LastQuery = Query;
        for *directory: DirectoriesToSearch {
        	if !directory.scanned then ScanDirectory(directory);
        }
        unlock(*TheMutex);

        if query_updated {
            log("Query updated: %", LastQuery);
            ExecuteQuery(LastQuery);
        }

        sleep_milliseconds(8);
    }

    return 0;
}

DrawQuerySelection :: (destination_rect: Rect) {
	scrollable_region_theme := theme.scrollable_region_theme;
	scrollable_region_theme.region_background.shape.rounding_flags = 0;
	scrollable_region, results_display_rect := begin_scrollable_region(destination_rect, *scrollable_region_theme);

	max_items := 250;//cast(s64) ceil(r.h / query_input_rect.h);

    result_item_rect := results_display_rect;
    result_item_rect.h = TheFont.character_height*1.5;
	result_item_rect.y -= scroll_region_value;

	files_drawn: [..]string;
	files_drawn.allocator = temp;

    lock(*TheMutex);
    for *QueryResults {
    	file := Files[it.file_index];
    	line := file.lines[it.line_index];

    	if !array_find(files_drawn, file.name) {
    		label(result_item_rect, file.path, *theme.label_theme);
    		result_item_rect.y += TheFont.character_height*1.5;
    		array_add(*files_drawn, file.name);
    	}

    	button_text := tprint("L%: %", it.line_index+1, trim(line.text));
        pressed := button(result_item_rect, button_text, *theme.button_theme, identifier = it_index);
        if pressed {
            SelectedQueryResult = it;
            ScrollToSelectedLine = true;
        }
        result_item_rect.y += TheFont.character_height*1.5;
        if it_index == max_items then break;
    }
    unlock(*TheMutex);

    end_scrollable_region(scrollable_region, 0, result_item_rect.y + result_item_rect.h, *scroll_region_value);
}

Query: string;
SelectedQueryResult: *QueryResult;
ScrollToSelectedLine := false;

AddSearchDirectory :: (path: string) {
	directory: DirectoryState;
	directory.path = path;
	lock(*TheMutex);
	array_add(*DirectoriesToSearch, directory);
	unlock(*TheMutex);
}

SettingsSubwindow: Subwindow_Info;
AddDirectoryInput: string;
DrawSettingsSubwindow :: (state: *Subwindow_State, r: Rect, data: *void) {
	add_directory_input_rect := r;
	add_directory_input_rect.h = xx TheFont.character_height*1.5;
	result_action, new_text, state := text_input(add_directory_input_rect, AddDirectoryInput, *theme.text_input_theme);
    if result_action & .TEXT_MODIFIED {
        // log("result_action: %, state.text=%", result_action, state.text);
        modify_string(*AddDirectoryInput, state.text);
    } else if result_action & .ENTERED {
    	directory_to_add := copy_string(AddDirectoryInput);
    	AddSearchDirectory(directory_to_add);
    	state.text = "";
    	modify_string(*AddDirectoryInput, "");
    }

    x_glyph_width := prepare_text(TheFont, "+") * 3;

	search_directory_list_rect := r;
	search_directory_list_rect.y += add_directory_input_rect.h;

	{
		scrollable_region_theme := theme.scrollable_region_theme;
	    scrollable_state, scrollabe_rect :=
	    	begin_scrollable_region(search_directory_list_rect, *scrollable_region_theme);

	    search_directory_label_rect := scrollabe_rect;
	    search_directory_label_rect.h = TheFont.character_height*1.5;
		search_directory_label_rect.y -= scroll_region_value_for_search_dirs;

		lock(*TheMutex);

		for directory: DirectoriesToSearch {
			label(search_directory_label_rect, directory.path, *theme.label_theme);
			search_directory_label_rect.y += search_directory_label_rect.h;
		}

		unlock(*TheMutex);

	    end_scrollable_region(scrollable_state, 0, search_directory_label_rect.y, *scroll_region_value_for_search_dirs);
	}

}

main :: () {
	tfh: Text_File_Handler;
	start_file(*tfh, "settings", "settings.txt");
	if tfh.failed {
		log_error("Failed to load settings");
		return;
	}

	Readmode :: enum {
		None;
		Filetypes;
		Directories;
	}

	mode := Readmode.None;
	line, found := consume_next_line(*tfh);
	while found {
		if line == ":: Filetypes" {
			mode = .Filetypes;
		} else if line == ":: Directories" {
			mode = .Directories;
		} else {
			if mode == {
				case .Filetypes; {
					filetype := copy_string(line);
					if !begins_with(filetype, ".") {
						modify_string(*filetype, sprint(".%", line));
					}
					array_add(*VaildFiletypes, filetype);
					log("Filetype: %", line);
				}
				case .Directories; {
					dir: DirectoryState;
					dir.path = copy_string(line);
					array_add(*DirectoriesToSearch, dir);
					log("Directory: %", dir.path);
				}
			}
		}
		line, found = consume_next_line(*tfh);
	}

	deinit(*tfh);

	array_add(*VaildFiletypes, ".jai");
	array_add(*VaildFiletypes, ".txt");
	array_add(*VaildFiletypes, ".c");
	array_add(*VaildFiletypes, ".h");
	array_add(*VaildFiletypes, ".cpp");
	array_add(*VaildFiletypes, ".kt");
	array_add(*VaildFiletypes, ".java");

    Query = copy_string("");
    AddDirectoryInput = copy_string("");

    thread_initialized := thread_init(*QueryThread, QueryThreadProc);
    if !thread_initialized {
        log_error("Failed to initialize query thread!");
        return;
    }
    thread_start(*QueryThread);

    init(*TheMutex);
    init(*QueryMutex);

    window := create_window(window_width, window_height, "Code Searcher");

	set_render_target(window, .LEFT_HANDED);

	ui_init();

    LoadFont();

    SettingsSubwindow.rect = get_rect(100, 100, 0.15 * window_width, TheFont.character_height*20.0);
	SettingsSubwindow.title_text = "Settings";
	SettingsSubwindow.draw = DrawSettingsSubwindow;
	SettingsSubwindow.open = false;

    last_loop_elapsed_time: float64 = 0;
    program_is_running := true;
    while program_is_running {
        time_at_beginning_of_loop := current_time_monotonic();

		update_window_events();

		for get_window_resizes() {
			update_window(it.window);

			if it.window == window {
                window_width  = it.width;
                window_height = it.height;

                LoadFont();
                theme.font = TheFont;
            }
		}

		for events_this_frame {
			if it.type == .QUIT {
				program_is_running = false;
			}
			getrect_handle_event(it);
		}

		clear_render_target(0.25, 0.25, 0.25, 1.0);
    	x, y, width, height := get_dimensions(window, true);
    	ui_per_frame_update(window, width, height, seconds_since_init());

    	base_theme := default_theme_procs[0]();
        theme = base_theme;
        theme.font = TheFont;
        theme.button_theme.font = theme.font;
        theme.button_theme.alignment = .Left;
        theme.label_theme.font = TheFont;
        theme.label_theme.alignment = .Left;
        theme.text_input_theme.font = TheFont;
        set_default_theme(theme);

        settings_button_rect: Rect;
        settings_button_rect.h = TheFont.character_height*1.5;
        settings_button_rect.w = prepare_text(TheFont, "Settings")*1.5;
        settings_button_rect.x = window_width - settings_button_rect.w;

        query_input_rect: Rect;
        query_input_rect.h = TheFont.character_height*1.5;
        query_input_rect.w = window_width - settings_button_rect.w;

        {
            result_action, new_text, state := text_input(query_input_rect, Query, *theme.text_input_theme);
            if result_action & .TEXT_MODIFIED {
                lock(*QueryMutex);
                modify_string(*Query, state.text);
                unlock(*QueryMutex);
                SelectedQueryResult = null;
            }
        }

		button_theme := theme.button_theme;
		button_theme.alignment = .Center;
        settings_button_pressed := button(settings_button_rect, "Settings", *button_theme);
        if settings_button_pressed {
        	SettingsSubwindow.open = true;
        }


        if SelectedQueryResult {
        	selected_file := Files[SelectedQueryResult.file_index];
			selected_line := selected_file.lines[SelectedQueryResult.line_index];

        	slidable_rect: Rect;
            slidable_rect.h = window_height - (query_input_rect.h);
            slidable_rect.w = xx window_width;
            slidable_rect.y = query_input_rect.y + query_input_rect.h;

			slidable_theme := *theme.slidable_region_theme;
			slidable_theme.orientation = .VERTICAL;
            slidable_state, first_rect, second_rect, divider_r := slidable_region(slidable_rect, slidable_theme);

			DrawQuerySelection(first_rect);

            switch_to_second_rect(slidable_state);

			{ // begin selection preview section
	            selection_preview_rect := second_rect;
	            selection_preview_rect.y += global_frame_thickness_in_pixels;
	            selection_preview_rect.h -= global_frame_thickness_in_pixels*2;

	            file_path_rect := selection_preview_rect;
				file_path_rect.h = xx TheFont.character_height;
	            label(file_path_rect, selected_file.path, *theme.label_theme);
				selection_preview_rect.y += (TheFont.character_height*1.5);
				selection_preview_rect.h -= (TheFont.character_height*1.5);

				{ // begin file contents region
					scrollable_region_theme := theme.scrollable_region_theme;
					scrollable_region_theme.region_background.shape.rounding_flags = 0;
					scrollable_region, file_contents_scrollable_rect := begin_scrollable_region(selection_preview_rect, *scrollable_region_theme);

					if ScrollToSelectedLine {
						y_value := SelectedQueryResult.line_index * TheFont.character_height;
						scroll_region_value_for_file_contents = xx y_value;
						ScrollToSelectedLine = false;
					}

					file_contents_line_rect := file_contents_scrollable_rect;
					file_contents_line_rect.h = xx TheFont.character_height;
					file_contents_line_rect.y -= scroll_region_value_for_file_contents;
					for line_index: 0..selected_file.lines.count-1 {
						line := selected_file.lines[line_index];

						if line_index == SelectedQueryResult.line_index {
							draw_procs.set_shader_for_color();
							p0 := Vector2.{ file_contents_line_rect.x, file_contents_line_rect.y };
							p1 := p0 + .{ file_contents_line_rect.w, 0 };
							p2 := p0 + .{ file_contents_line_rect.w, file_contents_line_rect.h };
							p3 := p0 + .{ 0, file_contents_line_rect.h };
							draw_procs.immediate_quad(
								p0, p1, p2, p3,
								color = theme.background_color_bright);
							draw_procs.immediate_flush();
						}
						label(file_contents_line_rect, trim_right(line.text), *theme.label_theme);
						file_contents_line_rect.y += file_contents_line_rect.h;
					}

					end_scrollable_region(scrollable_region, 0, file_contents_line_rect.y, *scroll_region_value_for_file_contents);
				} // end file contents region
			} // end selection preview section

			finish(slidable_state);
        } else {
            r: Rect;
            r.h = window_height - (query_input_rect.y + query_input_rect.h);
            r.w = xx window_width;
            r.y = query_input_rect.y + query_input_rect.h;

            DrawQuerySelection(r);
        }

        if SettingsSubwindow.open {
        	add_subwindow(*SettingsSubwindow, *theme.subwindow_theme);
        }

        draw_popups();

        swap_buffers(window);

		reset_temporary_storage();

        sleep_milliseconds(8);

        last_loop_elapsed_time = to_float64_seconds(current_time_monotonic() - time_at_beginning_of_loop);
	}
}

modify_string :: (ps: *string, new_value: string) {
    free(ps.*);
    ps.* = copy_string(new_value);
}
